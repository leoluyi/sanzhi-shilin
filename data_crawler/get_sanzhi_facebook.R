library(Rfacebook)
library(data.table)
library(magrittr)
library(parsedate)
library(bit64) # for bit64 data
library(parallel)
library(pbapply)
source("utils/fb_description_parser.R")

# get API token -----------------------------------------------------------

# https://developers.facebook.com/apps
# fb_oauth <- fbOAuth(app_id="1667711623447142",
#                     app_secret="****************",
#                     extended_permissions = TRUE)
# dir.create("./fb_oauth", showWarnings = FALSE)
# save(fb_oauth, file="./fb_oauth/fb_oauth")
load("./fb_oauth/fb_oauth")

# Get data ----------------------------------------------------------------

# 阮厝住三芝 https://www.facebook.com/groups/451870225491/about/

# Get all posts
posts <- getGroup("451870225491",
                  token=fb_oauth, n = 3000000,
                  since = as.Date("2015-01-01")) %>%
  setDT() %>%
  .[, created_date := parse_iso_8601(created_time)]

# Parse link
posts[is.na(message) & type != "status", 
      message := parse_description(link)]

# Get comments of posts ---------------------------------------------------

# posts <- fread("data/dt_sanzhi_fb.csv")
get_comments_ <- function(post_id, token) {
  # post_id = "451870225491_10159995520880492"
  # token = fb_oauth
  tryCatch({
    res <- getPost(post_id, token = token)
    messages <- res$comments$message  
    cat(messages)
    out <- paste(unique(messages), collapse = "\n")
  }, error = function(e) {
    # warning(e)
    out <<- NA
  })
  out
}

get_comments <- function(post_id, token) {
  # post_id = c(
  #   "451870225491_10159995520880492",
  #   "451870225491_10159995896015492",
  #   "451870225491_10159996537290492"
  # )
  cl <- makeCluster(detectCores() - 1)
  on.exit(stopCluster(cl))
  clusterExport(cl, c("getPost", "token"))
  
  out <- pbsapply(post_id, get_comments_, token, USE.NAMES = FALSE, cl = cl)
  out
}

posts[comments_count > 0, comments := get_comments(id, fb_oauth)]

# Export ------------------------------------------------------------------

# posts %>% fwrite("data/dt_sanzhi_fb.csv")
