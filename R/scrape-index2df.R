
































































index2df <- function(board, newest = 1, pages = NA,
                     search_term = NA, search_page = 1) {
  stopifnot(is.character(board),
            length(newest) == 1,
            is.numeric(search_page))

  board <- parse_board(board)
  search_case <- chk_idx_mode(board, newest, pages,
                              search_term, search_page)


  if (search_case == "custom_pages") {
    df0 <- custom_idx_url(board, pages)
  } else if (search_case == "term_search") {
    df0 <- term_idx_url(board, search_term, search_page)
  } else if (search_case == "newest_pages") {
    df0 <- newest_idx_url(board, newest)
  } else stop("No URLs found")




  idx_n <- as.integer(as.character(df0$idx_n))
  urls <- df0$url


  df <- vector("list", length = length(urls))

  for (i in seq_along(urls)) {
    temp <- get_index_info(urls[i])
    temp$idx_n <- idx_n[i]
    df[[i]] <- temp
  }
  df <- dplyr::bind_rows(df)

  return(df)
}







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




parse_board <- function(board) {
  if (str_detect(board, "^http")) {
    board <- str_remove(board, "^https://www.ptt.cc/bbs/") %>%
      str_remove("/.+$")
  }
  if (str_detect(board, ".html")) stop("Not a board URL")

  return(board)
}




board_search_error <- function(url) {
  not_found <- read_html2(url) %>%
    html_node("head") %>%
    html_node("title") %>%
    html_text() %>%
    str_detect("^404")
  error_message <- c("Page Not Found\n",
                     "Invalid board name or\n",
                     "Search range exceeding limits\n")
  if (not_found) stop(error_message)
}



custom_idx_url <- function(board, pages) {
  idx_n <- pages

  urls <- paste0("https://www.ptt.cc/bbs/", board,
                 "/index", idx_n, ".html")
  url_limit <- paste0("https://www.ptt.cc/bbs/", board,
                      "/index", max(idx_n), ".html")
  board_search_error(url_limit)

  df <- data_frame(idx_n, url = urls)
  return(df)
}



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
