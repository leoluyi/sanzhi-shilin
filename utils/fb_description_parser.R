library(httr)
library(pbapply)
library(rvest)
library(stringr)
library(magrittr)

parse_description <- function (url_list) {
  library(parallel)
  cl <- makeCluster(detectCores()-1); on.exit(stopCluster(cl))
  
  clusterExport(cl, c("get_des"))
  out <- pblapply(url_list, get_des) %>% unlist()
  print(length(out))
  
  out
}


get_des <- function (url) {
  # url <- "https://www.facebook.com/aboutkeelung/photos/a.199819080081200.58708.173996702663438/1078006878929078/?type=1"
  if (is.na(url) || length(url) != 1) return(NA)
  
  url_parsed <- httr::parse_url(url)
  is_fb_photo <- grepl("facebook.com", url_parsed$hostname) & grepl("\\/photos\\/", url_parsed$path)
  if (length(is_fb_photo) == 0) is_fb_photo <- FALSE
  
  tryCatch({
    if (identical(status_code(res), 200L)) {
      res <- GET(url,
                 add_headers(
                   # `accept`="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                   # `accept-encoding`="gzip, deflate, sdch",
                   # `accept-language`="zh-TW,zh;q=0.8,en;q=0.6,zh-CN;q=0.4,ja;q=0.2",
                   `upgrade-insecure-requests`="1",
                   `user-agent`="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36"
                 ))
      doc <- res %>% content(encoding = "UTF-8")
      
      description <- doc %>% 
        html_nodes('meta[name="description"]') %>% 
        html_attr("content") %>% 
        str_c(sep = " ")
      
      if (is.null(description) || length(description) == 0) return (NA)
      else description
    } else return (NA)
  }, error = function(e) {
    description <<- NA
  })
  
  description
}

