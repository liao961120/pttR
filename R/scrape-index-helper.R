






NULL


















get_index_url <- function(board_url) {

  board_newest_index <- read_html2(board_url) %>%
    html_nodes("div.btn-group") %>%
    html_nodes("a.btn.wide") %>%
    html_attr("href")

  board_name <- board_newest_index[2] %>%
    str_extract("bbs/.+$") %>%
    str_remove("^bbs/") %>%
    str_remove("/index.+$")

  index_num <- basename(board_newest_index[2]) %>%
    str_extract("[0-9]+") %>%
    as.integer() + 1

  index_url <- paste0("https://www.ptt.cc/bbs/", board_name,
                      "/index", index_num, ".html")

  return(c(index_num, index_url))
}





















get_index_info <- function(board_url) {

  raw2 <- read_html2(board_url) %>% html_nodes("div.r-ent")

  title <- raw2 %>% html_nodes("div.title") %>%
    html_text() %>%
    stringr::str_remove("^(\\n|\\t)+") %>%
    stringr::str_remove("(\\n|\\t)+$")


  del_idx <- which(str_detect(title,
                              "^\\(.*\u5df2\u88ab.*\u522a\u9664\\)"))

  if (length(del_idx) > 0) {
    title <- title[-del_idx]
    raw2 <- raw2[-del_idx]
  }

  pop <- raw2 %>% html_nodes("div.nrec") %>%
    html_text()
  pop[pop == ""] <- "0"

  category <- vapply(title, extr_post_category, "str")

  link <- raw2 %>% html_nodes("div.title") %>%
    html_nodes("a") %>% html_attr("href") %>%
    stringr::str_remove("^/bbs/")

  author <- raw2 %>% html_nodes("div.meta") %>%
    html_nodes("div.author") %>% html_text

  date <- raw2 %>% html_nodes("div.meta") %>%
    html_nodes("div.date") %>% html_text

  df <- dplyr::as_data_frame(cbind(pop, category, title,
              link, author, date))

  attr(df, "base_url") <- "https://www.ptt.cc/"
  return(df)
}







check_404 <- function(url, message) {
  not_found <- read_html2(url) %>%
    html_node("head") %>%
    html_node("title") %>%
    html_text() %>%
    stringr::str_detect("^404")

  if (not_found) stop(message)
}
