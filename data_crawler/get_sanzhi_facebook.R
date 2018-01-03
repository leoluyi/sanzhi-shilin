library(Rfacebook)
library(data.table)
library(magrittr)
library(parsedate)
source("utils/fb_description_parser.R")

# get API token -----------------------------------------------------------

# https://developers.facebook.com/apps
# fb_oauth <- fbOAuth(app_id="1667711623447142",
#                     app_secret="****************",
#                     extended_permissions = TRUE)
# dir.create("./fb_oauth", showWarnings = FALSE)
# save(fb_oauth, file="./fb_oauth/fb_oauth")

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

# Export ------------------------------------------------------------------

posts %>% fwrite("data/dt_sanzhi_fb.csv")
