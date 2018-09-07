library(dplyr)
library(stringr)

raw <- readr::read_csv(url("https://liao961120.github.io/PTT-scrapy/dict.csv"))

ptt_dict <- raw %>%
  filter(nchar(term) <= 8) %>%
  filter(!str_detect(term, "\\w+(、|；|x+|X+| )\\w+")) %>%
  filter(!str_detect(term, "^\\[.+\\]")) %>%
  filter(!str_detect(term, "^[a-zA-Z0-9]+$")) %>%
  mutate(term = str_remove_all(term, "^[、|「]")) %>%
  mutate(term = str_remove_all(term, "[」!?？！、，]$")) %>%
  mutate(term = str_remove(term, "^(\\.+|…+)")) %>%
  mutate(term = str_remove(term, "(\\.+|…+)$")) %>%
  filter(nchar(term) > 1) %>%
  distinct(term, .keep_all = T) %>%
  na.omit()

attr(ptt_dict, "date") <- Sys.Date()

saveRDS(ptt_dict, "data-raw/ptt_dict.rds")
