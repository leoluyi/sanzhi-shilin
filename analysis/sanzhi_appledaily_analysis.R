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
cutter <- function (text, seg_worker, filter_words) {
  # text = "馬英九去世新大學演講"
  if (text %in% c(".", "")) {
    return(NA_character_)
  }
  
  pattern <- sprintf("^%s", paste(filter_words, collapse = "|^"))
  tryCatch({
    text_seg <- seg_worker <= text
  }, error = function(e) {
    stop('"', text, '" >> ', e)
  })
  filter_seg <- text_seg[!stringr::str_detect(text_seg, pattern)]
  filter_seg
}

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

# unique word matrix
vocab <- create_vocabulary(text_token, ngram=c(1L, 3L), sep_ngram = "_")
pruned_vocab <- prune_vocabulary(
  vocab, term_count_min = 10, doc_proportion_max = 0.8,
  doc_proportion_min = 0.001, vocab_term_max = 20000)

# dtm
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(text_token, vectorizer)
# remove 1-word term
dtm <- dtm[, dtm %>% colnames() %>% nchar >= 2]

# dtm %>% find_freq_terms(30) # not good

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
key_term %>% head(100) %>% DT::datatable(extensions = "Responsive")

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


