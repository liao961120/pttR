#' Extract Post Category from Title
#'
#' @importFrom magrittr %>%
#' @importFrom  stringr str_detect str_match str_replace str_remove

extract_category <- function(post_title) {
  unesc <- function(...) stringi::stri_unescape_unicode(...)
  #esc <- function(...) stringi::stri_escape_unicode(...)

  str0 <- "\\u6a19\\u984c"                     # esc("標題")
  str1 <- paste0("^", unesc(str0), "\\[.+\\]") # "^標題\\[.+\\]"
  str2 <- paste0("^", unesc(str0), "Re: ")     # "^標題Re: "

  cond1 <- str_detect(post_title, str1)
  cond2 <- str_detect(post_title, str1)

  category <- str_match(post_title, str1)[1, 1] %>%
    str_remove(paste0("^", str0, "\\[")) %>%
    str_remove("\\]$")

  if (cond1) post_cat <- category
  if (cond2) post_cat <- "Re:"
  if (cond1 == F && cond2 ==F) post_cat <- NULL
}
