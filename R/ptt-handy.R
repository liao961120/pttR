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


#' Turn PTT board name to URL
#'
#' A wrapper of \code{\link[base]{paste0}} to turn board names
#' or post URLs with base URL removed back to URL.
#'
#' @param x Character. A board name or a partial URL (ending in
#'   \code{.html}) with base URL removed.
#' @param pre Character. A base URL. Defaults to the base URL of
#'   \href{PTT Web}{https://www.ptt.cc/bbs/}.
#'
#' @examples
#' as_url("gossiping")
#' as_url("Gossiping/M.1534490816.A.A3A.html")
#'
#' @export
as_url <- function(x, pre = "https://www.ptt.cc/bbs/") {
  if (stringr::str_detect(x, ".html$")) {
    x <- paste0(pre, x)
  } else {
    x <- paste0(pre, x, "/index.html")
  }

  return(x)
}




#' Return a data frame with popular boards info
#'
#' \code{hotboards} returns a data frame of
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
#' df <- hotboards()
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
hotboards <- function(get_new = FALSE) {
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
