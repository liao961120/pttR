#raw <- read_html("https://www.ptt.cc/bbs/Baseball/M.1535372407.A.902.html")
#raw <- read_html("https://www.ptt.cc/bbs/car/M.1500309737.A.D47.html")

#' @importFrom magrittr %>%
#' @importFrom rvest html_node html_text
#' @importFrom tibble data_frame as_data_frame
#' @importFrom Hmisc escapeRegex
#' @importFrom stringr str_remove str_remove_all str_replace str_extract_all str_extract
#' @keywords internal
mutate_cmt_reply <- function(comment_df, post_xml) {
  raw <- html_node(post_xml, "div#main-content") %>%
    html_text()
  if (length(raw) == 0) return(comment_df)

  ## Extract Reply
  reply <- raw %>%
    str_remove("(\n|.)+--\n\u203b \u767c\u4fe1\u7ad9") %>%
    str_remove_all("\\d{2}/\\d{2} \\d{2}:\\d{2}") %>%
    str_remove_all(reg_reply)

  ## Annotate all comments with number
  comment <- paste0(comment_df$user, ": ",
                    comment_df$comment)
  marked_reply <- reply
  for (i in seq_along(comment)) {
    marked_reply <- str_replace(
      marked_reply,
      paste0(escapeRegex(comment[i]),
             "(?!_CMT_\\d{1,4}_)"),
      paste0(comment[i], "_CMT_", i, "_")
    )
  }

  ## Find out comments with author replied
  replyed_cmt_num <- unlist(
    str_extract_all(marked_reply,
                    "_CMT_\\d{1,4}_ ?\n+(?!(\u63a8|\u5653|\u2192))")
  )

  ## Extract author replies and map to comment number
  replyed_cmt <- vector("character", length(replyed_cmt_num))
  for (i in seq_along(replyed_cmt_num)) {
    replyed_cmt[i] <- str_extract(marked_reply,
                                  paste0(replyed_cmt_num[i],
                                         "(.|\\s)*(?=(\u63a8|\u5653|\u2192|$))")) %>%
      str_remove("(\u63a8|\u5653|\u2192) [:alnum:]+: (\\s|.)+$") %>%
      str_remove("_CMT_\\d{1,4}_ ?\n+")
  }

  ## Combine author reply with comment numbers
  cmt_num <- as.integer(str_extract(replyed_cmt_num, "\\d{1,4}"))
  reply_df <- na.omit(data_frame(cmt_num = cmt_num,
                                 author_reply = replyed_cmt))

  ## Expand comment_df: add author reply column
  comment_df <- as.data.frame(comment_df)
  comment_df$author_reply <- ""
  comment_df$author_reply[reply_df$cmt_num] <- reply_df$author_reply

  return(as_data_frame(comment_df))
}

# str_detect("中文", "\\p{Ideographic}")

