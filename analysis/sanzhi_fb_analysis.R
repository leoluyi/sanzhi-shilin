library(magrittr)
library(data.table)
library(dplyr)
library(dtplyr)
library(readr)
library(stringr)
library(text2vec)
library(jiebaR) # word segmentation
library(wordcloud2)
library(ldatuning) # Select number of topics for LDA model # sudo apt install libmpfr-dev
library(wordVectors) # devtools::install_github("bmschmidt/wordVectors")
library(lda)
library(LDAvis)
library(ggplot2)
library(feather)
library(Matrix) # sparse matrix
library(slam) # sparse matrix
library(parsedate)
library(parallel)
# devtools::install_github("qinwf/ropencc") # 繁簡轉換
source("utils/filter_dtm.R", local = TRUE)
source("utils/seglist_to_dtm.R", local = TRUE)
source("utils/cutter.R", local = TRUE)
options(
  datatable.print.class = TRUE,
  datatable.print.topn = 20
)

# load data ---------------------------------------------------------------

dt_raw <- fread("data/dt_sanzhi_fb.csv")
# remove duplicate posts
dt_raw <- dt_raw %>% unique(by = "comments")


# EDA ---------------------------------------------------------------------

# 廣告文
dt_raw[, .(comments_count)] %>%
  ggplot(aes(comments_count)) + 
  geom_histogram() +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  theme_light()

dt_raw[comments_count > 12, .(message)][
  str_detect(message, "[貨買賣]|面交")] %>% View
# #現貨100組明天下午到貨 #大優惠快搶慢了就沒了喔‼️ #
# 三芝胖老爹生意超好 ，11開賣下午4點賣光了。我都沒吃到
# ＃老饕甘蔗烤雞最新出團時間
# ✨✨金山隱藏版鹹酥鴨✨✨ ✅到貨日12/29（五）取貨✅ ⚠
# 信杰寵物滿7歲 慶祝活動第二波來了 Flexi 飛萊希伸縮拉
# 三芝有一家魯味 下午5點左右開三個人在賣 很快就賣完了 但 
# 信杰寵物 7歲慶祝活動 第三波 2017.12.18 老闆重
# 三芝古早味純手工製作湯圓，家中母親製作，完全無防腐劑1斤70
# 親愛的三芝鄉親們，冬至將要來臨囉！這時候就要應景吃個湯圓囉！
# �手工牛軋餅� NT$150 - 三芝區 #現貨 #快速出貨
# 謝謝大家幫忙！我更新照片唷！ 各位鄉親！由於本人懶，所以半賣
# 請問靠近三芝國小附近有農舍要賣的嗎？要有院子喔。
# 可口美味雪Q餅

dt <- dt_raw[!str_detect(message, "[貨買賣]|面交") & comments_count <= 12]

#  Data cleansing ---------------------------------------------------------

url_re <- "http[s]?://(?:[a-zA-Z0-9$-_@.&+!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+(?:#[-A-z0-9]+)?"
emoji_re <- paste0("(\ud83d[\ude00-\ude4f])|",  # emoticons
                   "(\ud83c[\udf00-\uffff])|",  # symbols & pictographs (1 of 2)
                   "(\ud83d[\u0001-\uddff])|",  # symbols & pictographs (2 of 2)
                   "(\ud83d[\ude80-\udeff])|",  # transport & map symbols
                   "(\ud83c[\udde0-\uddff])") # flags (iOS)

dt[, `:=`(message = message %>% 
            str_replace_all(url_re, "") %>% 
            str_replace_all("\\p{So}|\\p{C}", "") %>%  # Unicode Regular Expressions
            str_replace_all("[^\u0020-\u007A\u4E00-\u9FFF]+", " "))]
dt[, `:=`(comments = comments %>% 
            str_replace_all(url_re, "") %>% 
            str_replace_all("\\p{So}|\\p{C}", "") %>% 
            str_replace_all("[^\u0020-\u007A\u4E00-\u9FFF]+", " "))]
# Remove empty news
dt <- dt[message != ""]

# tokenize ----------------------------------------------------------------

## 起手式，結巴建立斷詞器
mix_seg <- worker(type = "mix",
                  user = "dict/dict_utf8.txt",
                  stop_word = "dict/stop_utf8.txt",
                  symbol = FALSE,
                  encoding = "UTF-8")
hmm_seg <- worker(type = "hmm",
                  user = "dict/dict_utf8.txt",
                  stop_word = "dict/stop_utf8.txt",
                  symbol = FALSE,
                  encoding = "UTF-8")
# mix_seg <= post_text[1] # try first post

# self-made filter (built-in perl's regular expression has bug)
filter_words = readLines("dict/filter_words.txt")

# tokenize
text_seg <- dt[, paste(message, comments)] %>% lapply(cutter, mix_seg, filter_words)

# # tokenize in parallel
# cl <- makeCluster(detectCores()-1)
# clusterEvalQ(cl, {
#   library(stringr)
#   library(jiebaR)
#   mix_seg <- worker(type = "mix",
#                     user = "dict/dict_utf8.txt",
#                     stop_word = "dict/stop_utf8.txt",
#                     symbol = FALSE,
#                     encoding = "UTF-8")
# })
# clusterExport(cl, list("cutter", "mix_seg"), envir = )
# 
# text_seg <- dt[1:5, news_text] %>% 
#   parLapply(cl, ., cutter, mix_seg, filter_words) %>% 
#   parLapply(cl, ., function(x) x[!is.na(x)])
# 
# stopCluster(cl)

# adjust to the format for text2vec::itoken
text_token <- itoken(text_seg)


# 新詞探勘 ------------------------------------------------------------------

# Unique word matrix
vocab <- create_vocabulary(
  text_token, 
  ngram=c(1L, 2L),
  sep_ngram = "_"
)

pruned_vocab <- prune_vocabulary(
  vocab, 
  term_count_min = 5, 
  doc_proportion_min = 0.01, 
  doc_proportion_max = 0.9,
  vocab_term_max = 20000
)
# class(pruned_vocab) <- c("text2vec_vocabulary", "data.table", "data.frame")
pruned_vocab <- pruned_vocab[str_length(pruned_vocab$term) >= 2,] # remove 1-word term

# Make DTM
vectorizer <- vocab_vectorizer(pruned_vocab)
dtm <- create_dtm(text_token, vectorizer)

# Check most freq terms
dtm %>% colSums() %>% sort(decreasing = T) %>% head(20)

# 關鍵詞 Top 100 ------------------------------------------------------------

# 利用 tf-idf 關鍵詞算法，處理高頻詞高估及低頻詞低估的問題，取得整個文檔的關鍵詞

# tf-idf
tfidf = TfIdf$new() # define tfidf model
# fit model to train data and transform train data with fitted model
dtm_train_tfidf = fit_transform(dtm, tfidf)
# tfidf modified by fit_transform() call!

# Key term
key_term <- dtm_train_tfidf %>% 
  find_freq_terms(lowfreq = 0.05) %>%
  colSums() %>% 
  data.frame() %>% 
  data.table(keep.rownames = TRUE) %>% 
  setnames(c("keyword", "sum_tf_idf")) %>% 
  .[order(-sum_tf_idf)]
# key_term %>% head(100) %>% DT::datatable(extensions = "Responsive")

# Wordcloud
d <- key_term %>% head(150)
ncolor <- nrow(d)
get_palette = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2")) 
              # interpolate a set of given colors 
wordcloud2(d,
           size = 0.5,
           fontFamily = "Noto Sans CJK TC", 
           fontWeight = "normal",
           rotateRatio = 0,
           color = get_palette(ncolor),
           shape = "circle")


# Topic Models ------------------------------------------------------------

doc_list <- dt[, paste(message, comments)] %>% 
  lapply(cutter, mix_seg, filter_words) %>% 
  lapply(function(x) x[!is.na(x)]) 
dtm <- doc_list %>% seglist_to_dtm %>% filter_tfidf_dtm
# dtm <- dtm %>% as.simple_triplet_matrix

# compute the table of terms:
term_table <- dtm %>% slam::col_sums()
term_table <- sort(term_table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- term_table < 5 | names(term_table) %>% str_length == 1
term_table <- term_table[!del]
vocab <- names(term_table)

get_terms <- function(doc_list, vocab) {
  index <- match(doc_list, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc_list, get_terms, vocab=vocab)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term_table)  # frequencies of terms in the corpus

# 跑個模擬，挑一個好的主題數

# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
system.time({
  sim_result <- FindTopicsNumber(
    dtm,
    topics = c(seq(2, 6, by = 2),
               seq(10, 60, by = 5),
               seq(60, 100, by = 20)#,
               # seq(120, 200, by = 20)
    ),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 3L,
    verbose = TRUE
  )
}) # Time difference of 43.448 secs

FindTopicsNumber_plot(sim_result)

# MCMC and model tuning parameters:
K <- 20  # n_topic
G <- 3000 # num.iterations
alpha <- 0.02
eta <- 0.02

# Fit the model:
set.seed(357)
system.time({
  lda_fit <- lda.collapsed.gibbs.sampler(
    documents = documents, K = K, vocab = vocab, 
    num.iterations = G, alpha = alpha, 
    eta = eta, initial = NULL, burnin = 0,
    compute.log.likelihood = TRUE)
  # Save result
  saveRDS(lda_fit, file = "./models/lda_fit_fb.Rds")
})
#  user  system elapsed 
# 6.252   0.420   6.688

# Top topic result
top_docs_num <- lda_fit$document_sums %>% top.topic.documents(5)
top_words <- lda_fit$topics %>% top.topic.words(num.words = 5, by.score = TRUE) %>% 
  data.frame() %>% data.table()

top_words %>% 
 DT::datatable()


# LDAvis

theta <- t(apply(lda_fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(lda_fit$topics) + eta, 2, function(x) x/sum(x)))

# list(phi = phi,
#      theta = theta,
#      doc.length = doc.length,
#      vocab = vocab,
#      term.frequency = term.frequency)

lda_view <- list(phi = phi,
                 theta = theta,
                 doc.length = doc.length,
                 vocab = vocab,
                 term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi = lda_view$phi, 
                   theta = lda_view$theta, 
                   doc.length = lda_view$doc.length, 
                   vocab = lda_view$vocab, 
                   term.frequency = lda_view$term.frequency)
serVis(json, out.dir = 'output/ldavis_fb', open.browser = FALSE)

