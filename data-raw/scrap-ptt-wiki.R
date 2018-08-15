library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

## Links & Raw data
Links <- c(
  newest = "http://zh.pttpedia.wikia.com/index.php?title=%E7%89%B9%E6%AE%8A%3A%E6%96%B0%E9%A0%81%E9%9D%A2&namespace=0&username=&limit=500",
  board_name = "http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E7%9C%8B%E6%9D%BF",
  event_name = "http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E4%BA%8B%E4%BB%B6",
  famous_people = "http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E5%90%8D%E4%BA%BA",
  culture = "http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E6%96%87%E5%8C%96",
  notations = "http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E6%B5%81%E8%A1%8C%E7%AC%A6%E8%99%9F",
  basic_terms = "http://zh.pttpedia.wikia.com/wiki/%E5%88%86%E9%A1%9E:PTT%E5%9F%BA%E6%9C%AC%E7%94%A8%E8%AA%9E",
  post_type = "http://zh.pttpedia.wikia.com/wiki/PTT%E7%9A%84%E5%90%84%E9%A1%9E%E6%96%87%E7%AB%A0"
)

rawhtml <- vector("list", length = 8)
for (i in seq_along(rawhtml)) {
  rawhtml[[i]] <- read_html(Links[i])
}

## Helper Functions -------------------

# Extract link texts from 'Category Pages'
# Preprocess to rm parentheses and aka text
# in the same a tag text
extract_link_text <- function(rawhtml) {
  rawhtml %>%
    html_node("div#mw-pages") %>%
    html_nodes("li") %>%
    html_nodes("a") %>%
    html_text %>%
    paste(collapse = "_") %>%
    str_replace_all("、", "_") %>%
    str_replace_all("( [\\(（]|[\\(（])", "_") %>%
    str_replace_all("( [\\)）]|[\\)）])", "") %>%
    strsplit("_") %>%
    purrr::flatten_chr()
}

## Data From 'Newest Pages' ------------------

page_names <- rawhtml[[1]] %>%
  html_nodes("a.mw-newpages-pagename") %>%
  html_text() %>%
  str_remove("之亂")

page_date <- rawhtml[[1]] %>%
  html_nodes("span.mw-newpages-time") %>%
  html_text() %>%
  str_extract("^20[0-9]{2}年[0-9]{1,2}月[0-9]{1,2}日") %>%
  str_replace("年", "-") %>%
  str_replace("月", "-") %>%
  str_replace("日", "")

newest_df <- data_frame(value = page_names,
                        source = "newest",
                        date = as.Date(page_date,
                                       tz = "CST")
                        ) %>%
  mutate(value = str_remove(value,
                           "[ （\\(].+[）\\)]$"))

## Data from 'Ptt Board Name' -----------------
board_name <- extract_link_text(rawhtml[[2]]) %>%
  as_data_frame() %>%
  cbind(source = "board_name")


## Data from 'Ptt Event' -----------------
rm_term <- c("之亂$", "在ptt$", "在Ptt$", "在PTT$", "事件$", "與ptt$", " $")

event_name <- extract_link_text(rawhtml[[3]]) %>%
  str_remove(paste0(rm_term, collapse = "|")) %>%
  as_data_frame() %>%
  cbind(source = "event_name")

## Data from 'Ptt Famous People' -----------------
famous_people <- extract_link_text(rawhtml[[4]]) %>%
  as_data_frame() %>%
  cbind(source = "famous_people")

## Data from 'Ptt Culture' -----------------
culture <- extract_link_text(rawhtml[[5]]) %>%
  as_data_frame() %>%
  cbind(source = "culture")

## Data from 'Ptt Notations' -----------------
notations <- rawhtml[[6]] %>%
  html_node("div#mw-pages") %>%
  html_nodes("a") %>%
  html_text %>%
  append("崩╰(〒皿〒)╯潰") %>%
  as_data_frame() %>%
  cbind(source = "notations")



## Data from 'Ptt Basic Terms' -----------------
basic_terms <- extract_link_text(rawhtml[[7]]) %>%
  as_data_frame() %>%
  cbind(source = "basic_terms")

## Data from 'Ptt Post Type' ------------------
post_type <- rawhtml[[8]] %>%
  html_node("div.mw-content-text") %>%
  html_nodes("li") %>%
  html_nodes("a") %>%
  html_text() %>%
  as_data_frame() %>%
  cbind(source = "post_type")


## Combine all source --------------------
ptt_dict <- bind_rows(basic_terms,
                  board_name,
                  culture,
                  event_name,
                  famous_people,
                  notations,
                  post_type,
                  newest_df[ ,-3]) %>%
  rename("term" = value, "source" = source) %>%
  distinct(term, .keep_all = T)

attr(ptt_dict, "newest_page") <- newest_df$date[1]
# attr(ptt_dict, "newest_page")

devtools::use_data(ptt_dict, overwrite = T)
