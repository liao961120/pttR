#' Get PTT info
#'
#' Simple function with no arguments. Prints out
#' some usefull information about PTT.
#'
#' @importFrom magrittr %>%
#' @export
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
