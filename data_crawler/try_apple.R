library(httr)
library(rvest)
library(data.table)
library(lubridate)
library(stringr)

keyword = c("三芝")
date_from = "2017-01-01"
date_to = "2017-12-28"


keyword <- paste0(keyword, collapse = " AND ")
message(">> Searching keywords: ", keyword, "...")

date_from <- date_from %>%
  ymd(tz = "ROC") %>%
  strftime(tz = "ROC", "%Y%m%d")
date_from <- date_from %>%
  lubridate::ymd(tz = "ROC") %>%
  strftime(tz = "ROC", "%Y%m%d")
date_to <- date_to %>%
  lubridate::ymd(tz = "ROC") %>%
  strftime(tz = "ROC", "%Y%m%d")
date_int <- sprintf("[%s TO %s]", date_from, date_to)

## Get max page
max_page <- maxpage_appledaily(keyword, date_int)
if (max_page == -Inf) {
  message("No news result found.")
  return(invisible(NULL))
}
message(">> Getting ", max_page, " pages...")

# for (i in 1:max_page) {
#   cat(i, " ")
#   res <- search_appledaily_(keyword, i)
#   search_result <- rbindlist(list(search_result, res))
# }
if (.Platform$OS.type == "unix") {
  f <- function(keyword, i, date_int) {
    cat(i, " ") # to show page num in mclapply
    tryCatch({
      search_appledaily_(keyword, i, date_int)
    }, error = function(e) {
      print(e)
      data.table()
    })
  }
  res_list <- parallel::mclapply(X = seq_len(max_page),
                                 FUN = f,
                                 keyword = keyword,
                                 date_int = date_int,
                                 mc.cores = 1)
  # --- not paralleling => will get status code: 500
} else {
  stop("Unix-like system required")
}

search_result <- data.table(
  title = character(0L),
  description = character(0L),
  news_url = character(0L)
)
# print(c(list(search_result), res_list)) # for debug
search_result <- rbindlist(c(list(search_result), res_list))
search_result[, news_url := news_url %>%
                str_extract("^.*(?=/applesearch)")] #%>%  # normalize url
# .[, c("title", "description") := NULL] # drop unused columns

message("\n\n>> ", search_result %>% nrow, " news to load...")

out <- get_appledaily(search_result[, news_url], nb_core = "auto")

# Prevent inconsistent number of news, e.g., caused by connection error
# news_click <- search_result[res[, news_url], on = .(news_url)]
# out <- merge(news_click,
#              search_result, by = "news_url", all.x = TRUE)

message("\nDone! Got ", nrow(out), " news")
out

