library(Rfacebook)
library(data.table)
library(magrittr)

# get API token -----------------------------------------------------------

# https://developers.facebook.com/apps
# fb_oauth <- fbOAuth(app_id="1667711623447142",
#                     app_secret="****************",
#                     extended_permissions = TRUE)
# dir.create("./fb_oauth", showWarnings = FALSE)
# save(fb_oauth, file="./fb_oauth/fb_oauth")

# Get data ----------------------------------------------------------------

# Get all posts
posts_aboutkeelung <- getPage(page_aboutkeelung$username,
                              token=fb_oauth, n = 30000,
                              since = as.Date("2014-09-01")) %>%
  as_data_frame() %>%
  mutate(created_date = as.Date(created_time))

# Parse link
posts_aboutkeelung <- posts_aboutkeelung %>%
  mutate(link_description = parse_description(link)) %>%
  mutate(message_link = paste0(message, link_description, collapse = " "))

