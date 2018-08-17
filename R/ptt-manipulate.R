#' Extract Post Category from Title
#'
#' \code{extr_post_category} returns the category of a PTT post title.
#'
#' This function extracts the category from the title based on regular
#' expressions. It extracts the content inside the square brackets
#' (\code{[, ]}) of the title. If there is no square bracket,
#' it searches for \code{Re:} and extracts this pattern.
#' If neither is matched, the function returns
#' string \code{No}.
#'
#' @param post_title A string with UTF-8 encoding. No leading space
#'   in string.
#'
#' @return If there is square-bracket-match or \code{Re:}-match,
#'   the function returns a string.
#'   Else, the function returns string \code{No}.
#'
#' @examples
#'
#' \dontrun{
#' extr_post_category(str_err)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom  stringr str_detect str_match str_replace str_remove
extr_post_category <- function(post_title) {

  # Input Check
  if (str_detect(post_title, "^ +")) {
    stop("No leading space in post_title.")
  }

  unesc <- function(...) stringi::stri_unescape_unicode(...)
  #esc <- function(...) stringi::stri_escape_unicode(...)

  str0 <- unesc("\\u6a19\\u984c")     # "標題"
  contain_head <- str_detect(post_title, paste0("^", str0))
  if (contain_head) {
    post_title <- str_remove(post_title,  paste0("^", str0))
  }

  str1 <- "^\\[.+\\]" # "^標題\\[.+\\]"
  str2 <- "^Re: "     # "^標題Re: "

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
# str1 <- "[新聞] 金正男謀殺案今宣判 2女嫌律師:8成機率"
# str2 <- "Re: [新聞] 金正男謀殺案今宣判 2女嫌律師:8成機率"
# str_err <- " [新聞] 金正男謀殺案今宣判 2女嫌律師:8成機率"
#
# extr_post_category(str1)
# extr_post_category(str2)

