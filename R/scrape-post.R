



































































post2df <- function(path, board_col = FALSE) {


  posts_df <- vector("list", length = length(path))
  for (i in seq_along(path)) {
    posts_df[[i]] <- get_post(path[i])
  }
  posts_df <- bind_rows(posts_df)


  idx <- which(colnames(posts_df) == "content")

  posts_df <- mutate_content_len(posts_df, idx) %>%
     mutate_content_url(idx)

  return(posts_df)
}








mutate_content_len <- function(df, idx) {
  df$content_char <- df[, idx] %>%
    str_remove_all("( )") %>%
    str_remove_all("(\n)+") %>% nchar()
  return(df)
}




mutate_content_url <- function(df, idx, rpl = "rm_URL") {
  reg_url <- "(http[^ ]*)|(www\\.[^ ]*)"

  df$content_urls <- str_extract_all(df[, idx], reg_url)
  df[, idx] <- str_replace_all(df[, idx], reg_url, "rm_URL")

  return(df)
}




