#' Extract data from multiple index pages of a PTT board.
#'
#' \code{index2df} scrapes the index pages of a board
#' (\emph{\enc{看板}{kan ban}}) and extracts the
#' information into a data frame.
#'
#' @param board Character. Either a \strong{url} or a
#'   \strong{board name} that matches one of the entries
#'   in the variable \code{board} of the data frame
#'   returned by \code{\link{hotboards}}, such
#'   as \emph{"Gossiping"}, \emph{"Baseball"},
#'   \emph{"LoL"}.
#'   \strong{board name} is case-insensitive. See
#'   \strong{Examples} for details.
#'   \code{board} has a different requirements when used
#'   with argument \code{search} (See below).
#' @param n Numeric. Number of pages to scrape.
#'   Defaults to \code{1}, which scrapes only the
#'   newest page. If set to \code{2}, then scrapes
#'   the newest and the second-newest page, and so
#'   forth. \strong{The value should be kept low so
#'   that it doesn't put too much load on the server}.
#' @param range Numeric vector of length 2. When set,
#'   argument \code{n} has no effect. Defaults to \code{NA}.
#'   \describe{
#'     \item{The first element}{The starting page number of
#'       the board's index to retreive.}
#'     \item{The second element}{The ending page number of
#'       the board's index to retreive.}
#'   }
#' @param search Character vector of length 3. \code{search}
#'   works together with argument \code{board}, which in
#'   this case, \strong{can only be a board name} such as
#'   \emph{"Gossiping"} but \strong{not a url}. In addition,
#'   \code{board} is not limited to board names returned
#'   by \code{\link{hotboards}}. The only criterion
#'   is that \code{board} needs to match a real board name
#'   set as part of a PTT board url.
#'   \describe{
#'     \item{The first element}{A term (e.g. \emph{魯蛇}) to
#'       search on a PTT board.}
#'     \item{The second and third element}{Positive integers
#'       specifying the range to search. \code{1} indicates
#'       the newest page, numbers increases as the pages get
#'       older.}
#'    }
#'
#' @return Returns a data frame with one post info per
#'   row.
#'
#' @examples
#' # Get Board Name
#' head(hotboards())[, 1]
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
#' @seealso \code{\link{get_index_info}}
#'   \code{\link{get_index_info}} extracts data from
#'   \emph{one} index page, while \code{index2df} deals with
#'   \emph{several}. In addition, \code{index2df} has more
#'   functionality to deal with multiple pages extraction
#'
#' @importFrom stringr str_detect
#' @export
index2df <- function(board, n = 1, range = c(NA, NA),
                     search = c(term = NA, from = 1, to = 2)) {

  # Check and get urls
  if (!is.na(search[1])) {
    df <- check_index2df_search(board, search)
  } else {
    df <- check_index2df_order(board, n, range)
  }

  # IMPORTANT: coerce factor to chr,
  # or as.integer(factor) changes the value
  idx_n <- as.integer(as.character(df$idx_n))
  urls <- df$url

  # Combine individual dfs to one df
  df <- vector("list", length = length(urls))

  for (i in seq_along(urls)) {
    temp <- get_index_info(urls[i])
    temp$idx_n <- idx_n[i]
    df[[i]] <- temp
  }
  df <- dplyr::bind_rows(df)

  return(df)
}


##### Internal Helper Functions #####

#' Helper functions for scraping index pages
#'
#' These are helper functions for package development, not
#' written in man page index but available to advanced
#' users.
#'
#' @name scrape-index
NULL


#' Gets the latest index page url of a board
#'
#' \code{get_index_url} finds out the newest index page
#' of a board. It takes a board's url (e.g.
#' \url{https://www.ptt.cc/bbs/Gossiping/index.html})
#' as input and returns a character vector of length 2.
#'
#' @param board_url Character. A board's index page url.
#' @return \code{get_index_url} returns a char vector of
#'   length 2. The first element is a number,
#'   and the second is a url.
#' @rdname scrape-index
#'
#' @import rvest stringr
#' @export
#' @keywords internal
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



#' Gets the n newest index pages' urls of a board
#'
#' \code{get_index_urls} expands \code{\link{get_index_url}}
#' by returning multiple urls.
#'
#' @param board Character. Either a url or a board name
#'   that matches one of the entries in \code{board}
#'   of the data frame returned by
#'   \code{\link{hotboards}}. board name is
#'   case-insensitive.
#' @param n Numeric. Number of index page to retreive.
#'
#' @return \code{get_index_urls} returns a data frame with
#'   1 column of urls and 1 column of corresonding index page
#'   numbers. The number of rows equal the argument \code{n}.
#'
#' @rdname scrape-index
#'
#' @importFrom stringr str_replace
#' @export
#' @keywords internal
get_index_urls <- function(board, n) {
  if (n <= 0) stop("Invalid n.")

  board_url <- check_input_board(board)

  # newest index_num & url
  raw <- get_index_url(board_url)

  newest <- as.integer(raw[1])
  if (n > newest) stop("Requested number exceeds
                       total number of pages.")

  idx_n <- as.character(newest:(newest - n + 1))
  url <- rep(raw[2], n)

  df <- as.data.frame(cbind(idx_n, url))
  df$url <- str_replace(df$url, "index.*$",
                        paste0("index", df$idx_n, ".html"))
  return(df)
}



#' Extract message from a board's index page
#'
#' \code{get_index_info} takes a board's index url
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
get_index_info <- function(board_url) {
  raw <- read_html2(board_url) %>% html_nodes("div.r-ent")

  pop <- raw %>% html_nodes("div.nrec") %>%
    html_text()
  pop[pop == ""] <- "0"

  title <- raw %>% html_nodes("div.title") %>%
    html_text() %>%
    stringr::str_remove("^(\\n|\\t)+") %>%
    stringr::str_remove("(\\n|\\t)+$")

  category <- vapply(title, extr_post_category, "str")

  link <- raw %>% html_nodes("div.title") %>%
    html_nodes("a") %>% html_attr("href") %>%
    stringr::str_remove("^/bbs/")

  author <- raw %>% html_nodes("div.meta") %>%
    html_nodes("div.author") %>% html_text

  date <- raw %>% html_nodes("div.meta") %>%
    html_nodes("div.date") %>% html_text

  df <- dplyr::as_data_frame(cbind(pop, category, title,
              link, author, date))

  attr(df, "base_url") <- "https://www.ptt.cc/"
  return(df)
}
