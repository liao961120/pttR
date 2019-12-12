






ptt <- function() {
  base <- "https://www.ptt.cc/bbs/"
  wiki <- "http://zh.pttpedia.wikia.com/wiki/"
  shorturl <- "https://ppt.cc/"
  post <- "https://ppt.cc/f6sfCx"

  cat("Base URL\t", base, "\n")
  cat("URL Shortener\t", shorturl, "\n")
  cat("Example Post\t", post, "\n")
  cat("PTT Wiki\t", wiki, "\n")

  df <- head(hotboard_df[,-3], 8) %>% as.data.frame()
  df$link <- stringr::str_remove(df$link, base)
  colnames(df) <- NULL
  df
}

















as_url <- function(x, pre = "https://www.ptt.cc/bbs/") {
  if (TRUE %in% str_detect(x, ".html$")) {
    x <- paste0(pre, x)
  } else {
    x <- paste0(pre, x, "/index.html")
  }

  return(x)
}






























hotboards <- function(get_new = FALSE) {
  if (!get_new) return(hotboard_df)

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
    str_extract("^\u25ce\\[.+\\]") %>%
    str_remove("\u25ce\\[") %>%
    str_remove("\\]$")
  board_popularity <- hotboards %>%
    html_nodes("div.board-nuser > span") %>%
    html_text()
  df <- tibble::data_frame(board = board_name_en,
                           name_ch = board_name_ch,
                           popularity = board_popularity,
                           link = board_link)
  df$name_ch <- ifelse(is.na(df$name_ch),
                       df$board,
                       df$name_ch)
  attr(df, "date") <- Sys.time()

  return(df)
}




ping2zh <- function() pingying2zh



example_posts <- function() posts_df
