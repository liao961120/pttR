library(tidyr)
library(dplyr)
library(RCurl)
library(rvest)
library(stringr)
library(pttR)

### Generate board_info.rds ###

# Input: Non ("https://www.ptt.cc/bbs/hotboards.html")
# Output: data frame with 熱門看板資訊(name, chinese name,
# link to index)
get_hotboard_info <- function() {
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
  return(hotboard_df)
}

hotboard_df <- get_hotboard_info()
attr(hotboard_df, "date") <- Sys.time()


# Export data to RDS for Internal Usage

saveRDS(hotboard_df, "data-raw/hotboard_df.rds")
