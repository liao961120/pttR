library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
library(RCurl)

## Helper Functions -------------------

### Set cookie for 'over18'
curl_1 <- getCurlHandle()
curlSetOpt(cookie = "over18=1",
           followlocation = TRUE,
           curl = curl_1)

### Read Page for over 18 confirmation
read_html2 <- function(url, curl = curl_1) {
 getURL(url, curl = curl) %>% read_html()
}

### Get newest index for every board to
### determine the starting point of scraping
get_newest_index <- function(board_url) {

  board_newest_index <- read_html2(board_url) %>%
    html_nodes("div.btn-group") %>%
    html_nodes("a.btn.wide") %>%
    html_attr("href") %>%
    str_match("index[0-9]*.html")

  board_newest_index <- board_newest_index[2, 1] %>%
    str_extract("[0-9]+") %>%
    as.integer() + 1
  return(board_newest_index)
}

## Links to HotBoards -----------

# Source: https://www.ptt.cc/bbs/hotboards.html
link <- "https://www.ptt.cc/bbs/hotboards.html"
hotboards <- read_html(link) %>%
  html_nodes("div.b-ent") %>%
  html_nodes("a.board")

board_link <- hotboards %>%
  html_attr("href") %>%
  str_replace("^/", "https://www.ptt.cc/")

board_name_en <- hotboards %>%
  html_nodes("div.board-name") %>%
  html_text()

board_name_ch <- hotboards %>%
  html_nodes("div.board-title") %>%
  html_text() %>%
  str_extract("^◎\\[.+\\]") %>%
  str_remove("◎\\[") %>%
  str_remove("\\]$")

board_popularity <- hotboards %>%
  html_nodes("div.board-nuser > span") %>%
  html_text()

hotboard_df <- bind_cols(board = board_name_en,
                         name_ch = board_name_ch,
                         popularity = board_popularity,
                         link = board_link) %>%
  mutate(name_ch = ifelse(is.na(name_ch), board, name_ch))

## get Latest Page for every board ----------------

hotboard_df$newest_index <- NA
for (i in seq_along(hotboard_df$board[1:3])) {
  url <- hotboard_df$link[i]
  newest_index <- paste0("https://www.ptt.cc/bbs/",
                         hotboard_df$board[i], "/index",
                         get_newest_index(url), ".html")
  hotboard_df$newest_index[i] <- newest_index
  Sys.sleep(0.2)
}

# Set last updated
attr(hotboard_df, "date") <- Sys.Date()
# Write tp data-public/ for more access
readr::write_rds(hotboard_df,
                 "data-public/hotboard_df.rds")
# Export data to Package
devtools::use_data(hotboard_df, overwrite = T)


# To Do:
# 1. Work flow:
#   Efficiecntly scape web:
#     *1. Find Newest post
#     2. Fewest Request
# 2. Automate Collection

## Extract post data (in Gossiping) -------------------

### Helper Functions -----

# Extract Post category from title
extract_category <- function(post_title) {
  cond1 <- str_detect(post_title, "^標題\\[.+\\]")
  cond2 <- str_detect(post_title, "^標題Re: ")

  category <- str_match(post_title, "^標題\\[.+\\]")[1, 1] %>%
    str_remove("^標題\\[") %>%
    str_remove("\\]$")

  if (cond1) post_cat <- category
  if (cond2) post_cat <- "Re:"
  if (cond1 == F && cond2 ==F) post_cat <- NULL
}

### Main Function ----------
post1 <- read_html2("https://www.ptt.cc/bbs/Gossiping/M.1132970177.A.641.html")
post2 <- read_html2("https://www.ptt.cc/bbs/Gossiping/M.1534382109.A.E9D.html")
post3 <- read_html2("https://www.ptt.cc/bbs/Gossiping/M.1534388196.A.6C5.html")


## Meta info
post_meta <- post2 %>%
  html_nodes("div.article-metaline") %>%
  html_text()

post_author <- post_meta[1] %>%
  str_remove("^作者")

post_cat <- extract_category(post_meta[2])

post_title <- post_meta[2] %>%
  str_remove("^標題")

post_content <- post1 %>%
  html_node("div#main-content") %>%
  html_text() %>%
  # remove head portion
  str_remove("^(\n|.)*([0-9]{2}:){2}[0-9]{2} 20[0-9]{2}(\n)+") %>%
  # remove tail portion
  str_remove("(\n)+--\n※(\n|.)*")

