library(tm)

seglist_to_dtm <- function(seglist) {
  # seglist = comment_dt[, message]
  tmWordsVec <- sapply(seglist, function(w) paste(w, collapse = " "))
  ## build courpus
  myCorpus <- Corpus(VectorSource(tmWordsVec)) # build a corpus
  myCorpus <- tm_map(myCorpus, stripWhitespace) # remove extra whitespace
  dtm <- DocumentTermMatrix(
    myCorpus,
    control = list(
      wordLengths=c(2, Inf), # to allow long words
      removeNumbers = FALSE, ## if TRUE would remove 7-11
      weighting = weightTf,
      encoding = "UTF-8"
    ))
  # dtm <- removeSparseTerms(dtm, .99)
  dtm <- dtm[, !grepl("^\\d+$", dtm$dimnames$Terms)] ## remove numbers word
}
