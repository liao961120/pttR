library(dplyr)
library(stringr)

raw <- readr::read_csv(url("https://liao961120.github.io/PTT-scrapy/dict.csv"))

ptt_dict <- raw %>%
  filter(nchar(term) > 1) %>%
  distinct(term, .keep_all = T) %>%
  na.omit()

attr(ptt_dict, "date") <- Sys.Date()

readr::write_csv(ptt_dict[, 1], "inst/pttdict/pttdict.csv")
saveRDS(ptt_dict, "data-raw/ptt_dict.rds")
