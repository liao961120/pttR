






mutate_cmt_reply <- function(comment_df, post_xml) {
  if (is.null(comment_df)) return(comment_df)

  raw <- html_node(post_xml, "div#main-content") %>%
    html_text()


  reply <- raw %>%
    str_remove("(\n|.)+--\n\u203b \u767c\u4fe1\u7ad9") %>%
    str_remove_all("\\d{2}/\\d{2} \\d{2}:\\d{2}") %>%
    str_remove_all(reg_reply)


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


  replyed_cmt_num <- unlist(
    str_extract_all(marked_reply,
                    "_CMT_\\d{1,4}_ ?\n+(?!(\u63a8|\u5653|\u2192))")
  )


  replyed_cmt <- vector("character", length(replyed_cmt_num))
  for (i in seq_along(replyed_cmt_num)) {
    replyed_cmt[i] <- str_extract(marked_reply,
                                  paste0(replyed_cmt_num[i],
                                         "(.|\\s)*(?=(\u63a8|\u5653|\u2192|$))")) %>%
      str_remove("(\u63a8|\u5653|\u2192) [:alnum:]+: (\\s|.)+$") %>%
      str_remove("_CMT_\\d{1,4}_ ?\n+")
  }


  cmt_num <- as.integer(str_extract(replyed_cmt_num, "\\d{1,4}"))
  reply_df <- na.omit(data_frame(cmt_num = cmt_num,
                                 author_reply = replyed_cmt))


  comment_df <- as.data.frame(comment_df)
  comment_df$author_reply <- ""
  comment_df$author_reply[reply_df$cmt_num] <- reply_df$author_reply

  return(as_data_frame(comment_df))
}
