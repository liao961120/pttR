






















extr_post_category <- function(post_title) {


  if (str_detect(post_title, "^ +")) {
    stop("No leading space in post_title.")
  }

  unesc <- function(...) stringi::stri_unescape_unicode(...)


  str0 <- unesc("\\u6a19\\u984c")
  contain_head <- str_detect(post_title, paste0("^", str0))
  if (contain_head) {
    post_title <- str_remove(post_title,  paste0("^", str0))
  }

  str1 <- "^\\[.+\\]"
  str2 <- "^Re: "

  cond1 <- str_detect(post_title, str1)
  cond2 <- str_detect(post_title, str2)

  category <- str_match(post_title, str1)[1, 1] %>%
    str_remove("\\[") %>%
    str_remove("\\]$")

  if (cond1) post_cat <- category
  if (cond2) post_cat <- "Re:"
  if (cond1 == F && cond2 ==F) post_cat <- "No"

  return(post_cat)
}


