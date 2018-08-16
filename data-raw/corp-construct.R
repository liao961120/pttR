library(tidyr)
library(dplyr)
library(RCurl)

### Set cookie for 'over18'
curl_1 <- getCurlHandle()
curlSetOpt(cookie = "over18=1",
           followlocation = TRUE,
           curl = curl_1)

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

## Write tp data-public/ for more access -----------
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
