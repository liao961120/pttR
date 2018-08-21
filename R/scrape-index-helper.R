





##### Internal Helper Functions #####

#' Helper functions for scraping index pages
#'
#' These are helper functions for package development, not
#' written in man page index but available to advanced
#' users.
#'
#' @name scrape-index
NULL


#' Gets the latest index page URL of a board
#'
#' \code{get_index_url} finds out the newest index page
#' of a board. It takes a board's URL (e.g.
#' \url{https://www.ptt.cc/bbs/Gossiping/index.html})
#' as input and returns a character vector of length 2.
#'
#' @param board_url Character. A board's index page URL.
#' @return \code{get_index_url} returns a char vector of
#'   length 2. The first element is a number,
#'   and the second is a URL.
#' @rdname scrape-index
#'
#' @import rvest
#' @importFrom stringr str_remove str_extract str_match
#' @keywords internal
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

  index_url <- paste0("https://www.ptt.cc/", board_name,
                      "/", index_num, ".html")

  return(c(index_num, index_url))
}


#' Extract message from a board's index page
#'
#' \code{get_index_info} takes a board's index URL
#' as input and extract the content of the page into
#' a data frame with 6 variables.
#'
#' @return \code{get_index_info} returns a data frame
#'   with n rows and 6 variables, where n is the number
#'   of post links on an index page.
#'
#' @seealso \code{\link{extr_post_category}}
#'
#' @rdname scrape-index
#'
#' @import rvest
#' @export
#' @keywords internal
#' @export
get_index_info <- function(board_url) {
  raw2 <- read_html2(board_url) %>% html_nodes("div.r-ent")

  pop <- raw2 %>% html_nodes("div.nrec") %>%
    html_text()
  pop[pop == ""] <- "0"

  title <- raw2 %>% html_nodes("div.title") %>%
    html_text() %>%
    stringr::str_remove("^(\\n|\\t)+") %>%
    stringr::str_remove("(\\n|\\t)+$")

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


#' Check web page for 404 error
#'
#' @importFrom rvest html_node html_text
#' @importFrom magrittr %>%
#' @keywords internal
check_404 <- function(url, message) {
  not_found <- read_html2(url) %>%
    html_node("head") %>%
    html_node("title") %>%
    html_text() %>%
    stringr::str_detect("^404")

  if (not_found) stop(message)
}
