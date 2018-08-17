#' Return a data frame with popular boards info
#'
#' \code{get_board_popular} returns a data frame of
#' popular boards (熱門看板) on PTT.
#'
#' @examples
#' df <- get_board_popular()
#' head(df)
#'
#' @source \url{https://www.ptt.cc/bbs/hotboards.html}
#'
#' @export
get_board_popular <- function() {
  message("Web Scaped at: ", attr(hotboard_df, "date"))
  return(hotboard_df)
}



#' Convert a board's index pages to data frame
#'
#' \code{index2df} scrapes a board's (看板) index page
#' extracts information into a data frame.
#'
#' @param board A string. Either a url or a board name
#'   that matches one of the entries in the variable
#'   \code{board} of the data frame returned by
#'   \code{\link{get_board_popular}}. See
#'   \strong{Examples} for details.
#' @param  n Numeric. Number of pages to scrape.
#'   Defaults to \code{1}, which scrapes only the
#'   newest page. If set to \code{2}, then scrapes
#'   the newest and the second-newest page, and so
#'   forth. \strong{The value should be kept low so
#'   that it doesn't put too much load on the server}.
#'
#' @return Returns a data frame with one post info per
#'   row.
#'
#' @examples
#'
#' # Get Board Name
#' head(get_board_popular())[, 1]
#'
#' # Get data from 'Gossiping'
#' df <- index2df("Gossiping")
#'
#' \dontrun{
#' # Or use url directly
#' link <- "https://www.ptt.cc/bbs/Gossiping/index"
#'
#' df <- index2df(link, 3)
#' }
#'
#' @export
index2df <- function(board, n = 1) {

  urls <- get_index_urls(board, n)

  df <- vector("list", length = length(urls))
  for (i in seq_along(urls)) {
    df[[i]] <- get_index_info(urls[i])
  }
  df <- dplyr::bind_rows(df)

  return(df)
}



##### Internal Helper Functions #####


#' Get Latest Page url
#'
#' @param board_url url.
#' @return Character vector of length 2.
#'   The first element is a number,
#'   the second is a url.
#'
#' @import rvest stringr
get_index_url <- function(board_url) {

  board_newest_index <- read_html2(board_url) %>%
    html_nodes("div.btn-group") %>%
    html_nodes("a.btn.wide") %>%
    html_attr("href")

  index_num <- board_newest_index[2] %>%
    str_match("index[0-9]*.html") %>%
    str_extract("[0-9]+") %>%
    as.integer() + 1

  index_url <- paste0("https://www.ptt.cc",
                      board_newest_index[2])

  return(c(index_num, index_url))
}



#' Get the newest n urls of index page of a board
#'
#' @param board String. Either a url or a board name
#'   that matches one of the entries in \code{board}
#'   of the data frame returned by
#'   \code{\link{get_board_popular}}.
#' @param n Number of index page to retreive.
#'
#' @return Chracter vector with length equal to the
#'   argument \code{n}.
#'
#' @importFrom stringr str_replace
get_index_urls <- function(board, n) {

  # Input Check
  if (str_detect(board, "^http")) {
    board_url <- board
  } else if (sum(hotboard_df$board == board) == 1) {
    idx <- which(hotboard_df$board == board)
    board_url <- hotboard_df$link[idx]
  } else {
    stop("Only accept 'board url' or 'board name' matching
         get_board_popular()")
  }

  # newest index_num & url
  raw <- get_index_url(board_url)

  newest <- as.integer(raw[1])
  if (n > newest) stop("Requested number exceeds
                       total number of pages.")

  idx_n <- newest:(newest - n + 1)
  url <- rep(raw[2], n)

  df <- as.data.frame(cbind(idx_n, url))
  df$url <- str_replace(df$url, "index.*$",
                        paste0("index", df$idx_n, ".html"))
  return(df$url)
}



#' Convert a board's index page to data frame
#'
#' \code{get_index_info} takes a board's index url
#' as input and extract the content of the page into
#' a data frame with 6 variables.
#'
#' @param url A board's index page url.
#' @import rvest
get_index_info <- function(url) {
  raw <- read_html2(url) %>% html_nodes("div.r-ent")

  pop <- raw %>% html_nodes("div.nrec") %>%
    html_text() %>% as.integer()
  pop[which(is.na(pop))] <- 0

  title <- raw %>% html_nodes("div.title") %>%
    html_text() %>%
    stringr::str_remove("^(\\n|\\t)+") %>%
    stringr::str_remove("(\\n|\\t)+$")

  category <- vapply(title, extr_post_category, "str")

  link <- raw %>% html_nodes("div.title") %>%
    html_nodes("a") %>% html_attr("href")
  link <- paste0("https://www.ptt.cc", link)

  author <- raw %>% html_nodes("div.meta") %>%
    html_nodes("div.author") %>% html_text

  date <- raw %>% html_nodes("div.meta") %>%
    html_nodes("div.date") %>% html_text

  df <- cbind(pop, category, title,
              link, author, dates) %>%
    as.data.frame()
}
