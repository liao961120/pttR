#' Input Check for 'board'
#'
#' Check and get the index url of a board. If invalid,
#' stops executing and gives error message.
#'
#' @family helper-check_index2df_
#' @keywords internal
check_input_board <- function(board) {
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
           hotboards(get_new = FALSE)")
    }
  }
  return(board_url)
}


#' Check input and get urls for index2df(): search case
#'
#' @importFrom stringr str_detect
#' @importFrom utils URLencode head
#' @family helper-index2df
#' @keywords internal
check_index2df_search <- function(board, search) {
  if (str_detect(board, "^http")) stop("Invalid board name.")

  term <- search[1]
  idx_n <- as.character(search[2:3])
  idx_n <- as.integer(idx_n[1]):as.integer(idx_n[2])

  urls <- paste0("https://www.ptt.cc/bbs/", board,
                 "/search?page=", idx_n,
                 "&q=", URLencode(term))

  max_url <- paste0("https://www.ptt.cc/bbs/", board,
                    "/search?page=", max(idx_n),
                    "&q=", URLencode(term))
  check_404(max_url,
            message = c("Page Not Found\n",
                        "May due to invalid board name or\n",
                        "search range exceeding limits\n"))

  df <- data.frame(idx_n, url = urls)
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




#' Check input and get urls for index2df(): ordered case
#'
#' @family helper-index2df
#' @keywords internal
check_index2df_order <- function(board, n, range) {

  cond1 <- is.na(range[1]) && is.na(range[2])

  if (cond1) {
    index_df <- get_index_urls(board, n)
    urls <- index_df$url
    idx_n <- as.character(index_df$idx_n)
  } else if (!cond1) {
    # Get exact pages' urls set by 'range'
      if (is.na(range[1]) || is.na(range[2])) {
        stop("Arg. 'range' needs two integer")
      } else {
        url <- check_input_board(board)

        idx_n <- as.character(range[1]:range[2])
        url <- rep(url, length(idx_n))

        df <- as.data.frame(cbind(idx_n, url))
        df$url <- stringr::str_replace(df$url, "index.*$",
                                       paste0("index",
                                              df$idx_n, ".html"))
        urls <- df$url
      }
  }

  df <- data.frame(idx_n, url = urls)
  return(df)
}
