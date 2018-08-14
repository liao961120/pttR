library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

## Data From 'Newest Pages' ------------------
rawhtml <- read_html("http://zh.pttpedia.wikia.com/index.php?title=%E7%89%B9%E6%AE%8A%3A%E6%96%B0%E9%A0%81%E9%9D%A2&namespace=0&username=&limit=500")

page_names <- rawhtml %>%
  html_nodes("a.mw-newpages-pagename") %>%
  html_text() %>%
  str_remove("之亂")

page_date <- rawhtml %>%
  html_nodes("span.mw-newpages-time") %>%
  html_text() %>%
  str_extract("^20[0-9]{2}年[0-9]{1,2}月[0-9]{1,2}日") %>%
  str_replace("年", "-") %>%
  str_replace("月", "-") %>%
  str_replace("日", "")

newest_df <- data_frame(date = as.Date(page_date, tz = "CST"),
                        page = page_names) %>%
  mutate(page = str_remove(page, "[ （\\(].+[）\\)]$"))

## Data from 'Ptt Board Name' -----------------
rawhtml <- read_html("http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E7%9C%8B%E6%9D%BF")


## Data from 'Ptt Event' -----------------
rawhtml <- read_html("http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E4%BA%8B%E4%BB%B6")


## Data from 'Ptt Famous People' -----------------
rawhtml <- read_html("http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E5%90%8D%E4%BA%BA")


## Data from 'Ptt Culture' -----------------
rawhtml <- read_html("http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E6%96%87%E5%8C%96")


## Data from 'Ptt Notations' -----------------
rawhtml <- read_html("http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E6%B5%81%E8%A1%8C%E7%AC%A6%E8%99%9F")


## Data from 'Ptt Basic Terms' -----------------
rawhtml <- read_html("http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E5%9F%BA%E6%9C%AC%E7%94%A8%E8%AA%9E")


## Data from 'Ptt Post Categories'
rawhtml <- read_html("http://zh.pttpedia.wikia.com/wiki/PTT%E7%9A%84%E5%90%84%E9%A1%9E%E6%96%87%E7%AB%A0")

