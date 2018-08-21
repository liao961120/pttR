#' Extract data from multiple index pages of a PTT board.
#'
#' \code{index2df} scrapes the index pages of a board
#' (\emph{\enc{看板}{kan ban}}) and extracts the
#' information into a data frame.
#'
#' @param board Character. Either a \strong{url} or a
#'   \strong{board name}, such as \emph{"Gossiping"},
#'   \emph{"Baseball"}, \emph{"LoL"}.
#'   \strong{board name} is case-insensitive. See
#'   \strong{Examples} for details.
#'   \code{board} has a different requirements when used
#'   with argument \code{search} (See below).
#' @param newest Integer. Number of pages, starting from
#'   the most recent page, to scrape.
#'   Defaults to \code{1}, which scrapes only the
#'   newest page. If set to \code{2}, then scrapes
#'   the newest and the second-newest page, and so
#'   forth.
#' @param pages Integer vector. A vector of index page number(s).
#'   This parameter lets you scrape the index pages
#'   you want, provided that the page exist. Becareful not to
#'   provide numbers exceeding the range of current index pages.
#'   Defaults to \code{NA}.
#' @param search_term Character. A term to search in the index,
#'   such as "\emph{魯蛇}". There are also some advanced
#'   search methods:
#'   \describe{
#'   \item{Post thread}{Prepend "\emph{thread:}" to the
#'   search term (post title), e.g.
#'   "\emph{thread:抱怨「垃圾不分藍綠」姚文智：害民進黨被}"
#'   .}
#'   \item{Posts of an author}{Prepend "\emph{author:}"
#'   to the author's ID, e.g., "\emph{author:Plumage}".}
#'   }
#' @param search_page Integer vector. A vector of index page
#'   number(s). With argument \code{search_term}) set,
#'   \code{search_page} lets you to scrape index pages related
#'   to a specific term. Defaults to \code{1}, which scrapes only
#'   the newest page.
#'
#' @return A data frame with one post info per row.
#'
#' @examples
#' # Get data from 'Gossiping'
#' index_df <- index2df("Gossiping")
#' head(index_df)
#'
#' \dontrun{
#' # Or use url directly
#' link <- "https://www.ptt.cc/bbs/Gossiping/index"
#'
#' index_df <- index2df(link)
#' }
#' @section Warning:
#' \strong{Do not request too many pages one time}.
#' It places heavy load on the server.
#'
#' @seealso \code{\link{get_index_info}}
#'   \code{\link{get_index_info}} extracts data from
#'   \emph{one} index page, while \code{index2df} deals with
#'   \emph{several}. In addition, \code{index2df} has more
#'   functionality to deal with multiple pages extraction
#'
#' @importFrom stringr str_detect str_remove
#' @export
index2df <- function(board, newest = 1, pages = NA,
                     search_term = NA, search_page = 1) {
  stopifnot(is.character(board),
            length(newest) == 1,
            is.numeric(search_page))

  board <- parse_board(board)
  search_case <- chk_idx_mode(board, newest, pages,
                              search_term, search_page)

  # Return df with idx and urls
  if (search_case == "custom_pages") {
    df0 <- custom_idx_url(board, pages)
  } else if (search_case == "term_search") {
    df0 <- term_idx_url(board, search_term, search_page)
  } else if (search_case == "newest_pages") {
    df0 <- newest_idx_url(board, newest)
  } else stop("No urls found")


  # IMPORTANT: coerce factor to chr,
  # or as.integer(factor) changes the value
  idx_n <- as.integer(as.character(df0$idx_n))
  urls <- df0$url

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


# Helper Functions of index2df ------
# Input Checking --------------

## Determine the mode of index2df
## One of: "custom_pages", "term_search", "newest_pages", stop
chk_idx_mode <- function(board, newest, pages,
                             search_term, search_page) {
  if (!(TRUE %in% is.na(pages))) {
    cat("Searching custom range of index pages ...\n")
    case <- "custom_pages"
  } else if (!(TRUE %in% is.na(search_term))) {
    cat("Searching ", board, " for '",
        search_term, "' ...\n", sep="")
    case <- "term_search"
  } else if (is.numeric(newest)) {
    if (newest <= 0) stop("Arg. 'newest' must be a positive int")
    cat("Searching the newest", newest, "page(s) of",
        board ,"...\n")
    case <- "newest_pages"
  } else stop("Input error.\n")

  return(case)
}

## Parse board input
#' @importFrom stringr str_detect str_remove
parse_board <- function(board) {
  if (str_detect(board, "^http")) {
    board <- str_remove(board, "^https://www.ptt.cc/bbs/") %>%
      str_remove("/.+$")
  }
  if (str_detect(board, ".html")) stop("Not a board url")

  return(board)
}

## Page Not Found error
#' @importFrom magrittr %>%
#' @importFrom rvest html_node html_text
board_search_error <- function(url) {
  not_found <- read_html2(url) %>%
    html_node("head") %>%
    html_node("title") %>%
    html_text() %>%
    stringr::str_detect("^404")
  error_message <- c("Page Not Found\n",
                     "Invalid board name or\n",
                     "Search range exceeding limits\n")
  if (not_found) stop(error_message)
}

# Constructing a data frame of urls and index_page_nums -----
## Search by custom pages
custom_idx_url <- function(board, pages) {
  idx_n <- pages

  urls <- paste0("https://www.ptt.cc/bbs/", board,
                 "/index", idx_n, ".html")
  url_limit <- paste0("https://www.ptt.cc/bbs/", board,
                      "/index", max(idx_n), ".html")
  board_search_error(url_limit)

  df <- tibble::data_frame(idx_n, url = urls)
  return(df)
}

## Search by term
#' @importFrom utils URLencode head
term_idx_url <- function(board, search_term, search_page) {
  idx_n <- search_page
  term <- search_term

  urls <- paste0("https://www.ptt.cc/bbs/", board,
                 "/search?page=", idx_n,
                 "&q=", URLencode(term))
  url_limit <- paste0("https://www.ptt.cc/bbs/", board,
                    "/search?page=", max(idx_n),
                    "&q=", URLencode(term))
  board_search_error(url_limit)

  df <- tibble::data_frame(idx_n, url = urls)
  return(df)
}

## Search newest index
newest_idx_url <- function(board, newest) {
  idx_url <- get_index_url(paste0("https://www.ptt.cc/bbs/",
                                  board, "/index.html"))
  cat("The newest index page of '", board, "' is ", idx_url[1],
      ".\n", idx_url[2], "\n", sep = "")

  idx_n <- seq(from = as.integer(idx_url[1]),
               to = as.integer(idx_url[1]) - newest + 1)
  if (min(idx_n) <= 0) stop("Range exceeds possible.")

  urls <- paste0("https://www.ptt.cc/bbs/", board,
               "/index", idx_n, ".html")

  df <- tibble::data_frame(idx_n, url = urls)
  return(df)
}
