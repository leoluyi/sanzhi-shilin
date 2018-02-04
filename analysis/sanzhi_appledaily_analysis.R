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

dt_raw <- fread("data/dt_sanzhi_appledaily_2016_2017.csv")
# remove duplicate news
dt_raw <- dt_raw %>% unique(by = "title")
setindex(dt_raw, keywords)
dt_raw[, .N, keywords][order(-N)]
#    keywords   N
# 1:     生活 143
# 2:     社會  82
# 3:     副刊  34
# 4:     娛樂  30
# 5: 頭條要聞  29
# 6:     政治  16
# 7:     財經   6
# 8:     地產   4
# 9:     論壇   4
#10:     搜奇   4
#11:     動物   3
#12:     體育   2

dt_raw[keywords == "生活", .(title)]
dt_raw[keywords == "副刊", .(title)]


# 老人 - 相關新聞
dt_raw[str_detect(news_text, "老[翁人化]"), 
       .(title, datetime, str_extract_all(news_text, "老[翁人化]"))]

# 長照 - 相關新聞
dt_raw[str_detect(news_text, "長照"), 
       .(title, str_extract_all(news_text, ".{6}長照.{6}"))]

dt <- dt_raw[keywords %in% c("頭條要聞", "論壇", "副刊", "財經", "生活")]
# dt <- dt_raw[keywords %in% c("副刊", "生活")]

#  Data cleansing ---------------------------------------------------------

url_re <- "(https?|ftp)://[^\\s/$.?#].[^\\s]"

dt[, news_text := news_text %>% 
     str_replace_all(url_re, "") %>% 
     str_replace_all("\\p{So}|\\p{C}", "")] # Unicode Regular Expressions
dt[, news_text := news_text %>% str_replace_all(url_re, "")]
# Remove author
dt[, news_text := news_text %>%
     str_replace("([【(（]).*?(更新|新增|報導).*?[】)）]", "")]
# Remove empty news
dt <- dt[news_text != ""]

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
text_seg <- dt[, news_text] %>% lapply(cutter, mix_seg, filter_words)

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
  term_count_min = 15, 
  doc_proportion_min = 0.05, 
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
d <- key_term %>% head(200)
ncolor <- nrow(d)
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
wordcloud2(d,
           size = 0.5,
           fontFamily = "Noto Sans CJK TC", 
           fontWeight = "normal",
           rotateRatio = 0,
           color = getPalette(ncolor),
           shape = "circle")


# Topic Models ------------------------------------------------------------

doc_list <- dt[, news_text] %>% 
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
  result <- FindTopicsNumber(
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
}) # Time difference of 3.317039 mins

FindTopicsNumber_plot(result)

# MCMC and model tuning parameters:
K <- 45  # n_topic
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
  saveRDS(lda_fit, file = "./models/lda_fit_appledaily.Rds")
})
#   user  system elapsed 
# 77.780   0.552  78.693 

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
serVis(json, out.dir = 'output/ldavis_appledaily', open.browser = FALSE)

