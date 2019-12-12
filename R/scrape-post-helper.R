






































get_post <- function(post_url, board_col = FALSE) {

  post_xml <- read_html2(post_url)

  post_meta <- get_post_meta(post_xml,
                             board_col = board_col)
  post_content <- get_post_content(post_xml)
  post_comment <- mutate_cmt_reply(
    get_post_comment(post_xml), post_xml
    )
  if (is.null(post_comment)) {
    n_comment <- 0
    n_push <- 0
    n_boo <- 0
  } else {
    n_comment <- nrow(post_comment)
    n_push <- sum(post_comment$tag == "Push")
    n_boo <- sum(post_comment$tag == "Boo")
  }
  comment_meta <- cbind(n_comment, n_push, n_boo) %>%
    as.data.frame()

  if (str_detect(post_url, "^http")) {
    post_url <- str_remove(post_url, "^https://www.ptt.cc/bbs/")
  } else {
    post_url <- basename(post_url)
  }

  post_df <- bind_cols(post_meta, post_content,
                       comment = NULL, comment_meta,
                       link = post_url)
  post_df <- as.data.frame(post_df)

  post_df$comment[[1]] <- post_comment

  return(post_df)
}











































get_post_meta <- function(post_xml, board_col = FALSE) {

  post_meta <- post_xml %>%
    html_nodes("div.article-metaline") %>%
    html_text()

  post_author <- post_meta[1] %>%
    str_remove("^\u4f5c\u8005")
  post_cat <- extr_post_category(post_meta[2])
  post_title <- post_meta[2] %>%
    str_remove("^\u6a19\u984c")

  post_date <- parse_post_date(post_xml)

  tag <- "div.article-metaline-right > span.article-meta-value"
  post_board <- post_xml %>%
    html_nodes(tag) %>% html_text()

  post_meta <- bind_cols(author = post_author,
                         category = post_cat,
                         title = post_title,
                         date = post_date)

  if (board_col == T) {
    post_meta <- bind_cols(board = post_board,
                           post_meta)
  }

  return(post_meta)
}

























get_post_content <- function(post_xml) {

  post_content <- post_xml %>%
    html_node("div#main-content") %>%
    html_text() %>%
    str_remove("^(\n|.)*([0-9]{2}:){2}[0-9]{2} 20[0-9]{2}(\n)+") %>%
    str_remove("(\n)+--\n\u203b(\n|.)*")

  post_content <- data_frame(content = post_content)
  return(post_content)
}











































get_post_comment <- function(post_xml) {

  push <- post_xml %>% html_nodes("div.push")

  if (length(push) == 0) return(NULL)

  push_tag <- push %>%
    html_nodes("span.push-tag") %>%
    html_text() %>%
    str_replace_all("\u2192 ", "Neu") %>%
    str_replace_all("\u63a8 ", "Push") %>%
    str_replace_all("\u5653 ", "Boo")

  push_user <- push %>%
    html_nodes("span.push-userid") %>%
    html_text()

  push_content <- push %>%
    html_nodes("span.push-content") %>%
    html_text() %>%
    str_remove("^: ")

  push_time <- push %>%
    html_nodes("span.push-ipdatetime") %>%
    html_text() %>%
    as_data_frame() %>%
    separate("value", into = c("ip", "date"), sep = -12)

  push_time$ip <- str_remove(push_time$ip, "^ +")
  push_time$ip <- str_remove(push_time$ip, " +$")
  push_time$date <- str_remove(push_time$date, "\n$")

  push_df <- bind_cols(tag = push_tag,
                       user = push_user,
                       comment = push_content,
                       ip = push_time$ip,
                       time = push_time$date)

  post_date <- parse_post_date(post_xml) %>%
    ymd_hms(tz = "Asia/Taipei")

  push_df <- parse_comment_date(push_df, post_date)

  return(push_df)
}
