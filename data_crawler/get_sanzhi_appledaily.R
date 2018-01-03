library(newsr)
library(data.table)

dt_sanzhi <- search_appledaily(
  keyword = c("三芝"),
  date_from = "2016-01-01",
  date_to = "2017-12-28"
)

dt_sanzhi %>% 
  fwrite("data/dt_sanzhi_appledaily_2016_2017.csv")
