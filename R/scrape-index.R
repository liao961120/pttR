#' Return a data frame with popular boards info
#'
#' \code{get_hotboard_info} returns a data frame of
#' popular boards (\emph{\enc{熱門看板}{re men kan ban}})
#' on PTT.
#'
#' @param get_new Logical. Defaults to \code{FALSE}.
#'   If \code{TRUE}, scrapes and retreive data from
#'   \url{https://www.ptt.cc/bbs/hotboards.html}.
#'   If \code{FALSE}, use pre-scraped data stored in
#'   the package.
#'
#' @examples
#' df <- get_hotboard_info()
#' head(df)
#'
#' # Get data update time
#' attr(df, "date")
#'
#' @source \url{https://www.ptt.cc/bbs/hotboards.html}
#'
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom xml2 read_html
#' @importFrom stringr str_replace str_extract str_remove
#' @importFrom dplyr %>% bind_cols mutate
#' @export
get_hotboard_info <- function(get_new = FALSE) {

  if (!get_new) return(hotboard_df)

  link <- "https://www.ptt.cc/bbs/hotboards.html"

  hotboards <- read_html(link) %>%
    html_nodes("div.b-ent") %>%
    html_nodes("a.board")

  board_link <- hotboards %>%
    html_attr("href") %>%
    str_replace("^/", "https://www.ptt.cc/")

  board_name_en <- hotboards %>%
    html_nodes("div.board-name") %>%
    html_text()

  board_name_ch <- hotboards %>%
    html_nodes("div.board-title") %>%
    html_text() %>%
    str_extract("^\u25ce\\[.+\\]") %>% # \u25ce : ◎
    str_remove("\u25ce\\[") %>%
    str_remove("\\]$")

  board_popularity <- hotboards %>%
    html_nodes("div.board-nuser > span") %>%
    html_text()

  df <- cbind(board = board_name_en,
              name_ch = board_name_ch,
              popularity = board_popularity,
              link = board_link)
  df$name_ch <- ifelse(is.na(df$name_ch),
                                df$board,
                                df$name_ch)

  attr(df, "date") <- Sys.time()

  return(df)
}



#' Convert a board's index pages to data frame
#'
#' \code{index2df} scrapes index pages of a board
#' (\emph{\enc{看板}{kan ban}}) and extracts
#' information into a data frame.
#'
#' @param board Character. Either a \strong{url} or a
#'   \strong{board name} that matches one of the entries
#'   in the variable \code{board} of the data frame
#'   returned by \code{\link{get_hotboard_info}}, such
#'   as \emph{"Gossiping"}, \emph{"Baseball"}, \emph{"LoL}.
#'   \strong{board name} is case-insensitive. See
#'   \strong{Examples} for details.
#' @param n Numeric. Number of pages to scrape.
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
#' head(get_hotboard_info())[, 1]
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
#'   \code{\link{get_hotboard_info}}. board name is
#'   case-insensitive.
#' @param n Numeric. Number of index page to retreive.
#'
#' @return \code{get_index_urls} returns a chr vector of
#'   urls with length equal to the argument \code{n}.
#'
#' @rdname scrape-index
#'
#' @importFrom stringr str_replace
#' @export
#' @keywords internal
get_index_urls <- function(board, n) {

  # Input Check
  if (str_detect(board, "^http")) {
    board_url <- board
  } else {
    board <- tolower(board)
    board_names <- tolower(hotboard_df$board)

    cond <- sum(board_names == board) == 1 # Exactly 1 match
    if (cond) {
      idx <- which(board_names == board)
      board_url <- hotboard_df$link[idx]
    } else {
      stop("Only accept 'board url' or 'board name' matching
           get_hotboard_info(get_new = FALSE)")
    }
  }

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
  return(df$url)
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
    html_nodes("a") %>% html_attr("href")
  link <- paste0("https://www.ptt.cc", link)

  author <- raw %>% html_nodes("div.meta") %>%
    html_nodes("div.author") %>% html_text

  date <- raw %>% html_nodes("div.meta") %>%
    html_nodes("div.date") %>% html_text

  df <- dplyr::as_data_frame(cbind(pop, category, title,
              link, author, date))

  return(df)
}
