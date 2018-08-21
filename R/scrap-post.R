#' Read PTT pages with "over18-confirmation"
#'
#' A wrapper of \code{xml2::read_html()} with cookie set
#' to bypass PTT's "over18-confirmation".
#'
#' @param url Character. The url of the target web page.
#' @param ... Additional arguments passed on to
#'   \code{\link[xml2]{read_html}}.
#'
#' @return An XML document. See \code{\link[xml2]{read_html}}
#'   for more information.
#'
#' @examples
#' url <- "https://www.ptt.cc/bbs/Gossiping/index.html"
#' read_html2(url)
#'
#' @importFrom RCurl getCurlHandle curlSetOpt getURL
#' @importFrom xml2 read_html
#' @export
read_html2 <- function(url, ...) {
 curl_1 <- getCurlHandle()
 curlSetOpt(cookie = "over18=1",
                   followlocation = TRUE,
                   curl = curl_1)
 url2 <- getURL(url, curl = curl_1)
 read_html(url2, ...)
}


#' Get all information from an individual PTT post
#'
#' \code{get_post} returns a data frame with 1 row and 9 cols,
#' where the column \code{comment} is a list column.
#'
#' This is a function that combines the data gathered from
#' three functions: \code{\link{get_post_meta}},
#' \code{\link{get_post_content}}, and
#' \code{\link{get_post_comment}}.
#'
#' @param post_url Character. An url of a PTT post.
#' @param index Integer. The number of the index page that has
#'   \code{post_url}. If given, creates a new column,
#'   \code{index}.
#' @param board_col Logical. Whether to set board name as a
#'   variable. Defaults to \code{FALSE}. Note you can get the
#'   board name with \code{attributes(df)$board} or
#'   \code{attr(df, "board")} regardless of the value of this
#'   argument.
#'
#' @return A data frame with 1 row and 10 variables:
#'   \describe{
#'     \item{author}{Author of the post.}
#'     \item{category}{Category of the post, such as
#'       \emph{\enc{新聞}{xin wen}},
#'       \emph{\enc{問卦}{wen gua}}, \emph{Re:}.}
#'     \item{title}{Title of the post.}
#'     \item{date}{The date of the post.}
#'     \item{content}{The content of the post.}
#'     \item{n_char}{The Number of characters in the post content.
#'       Whitespaces and newline characters are removed before
#'       counting.}
#'     \item{comment}{A list column.
#'       See \code{\link{get_post_comment}} for information
#'       about entries in this list column.}
#'     \item{n_comment}{Number of comments.}
#'     \item{n_push}{Number of "Push" comments.}
#'     \item{n_boo}{Number of "Boo" comments.}
#'     \item{link}{url of the post with base url removed,
#'       which is \url{https://www.ptt.cc/bbs/}. Get the base
#'       url with \code{attr(df, "base_url")}.}
#'   }
#'   Two additional variables are optional:
#'   \describe{
#'     \item{board}{The board the post belongs to. Exist only
#'       if \code{board = TRUE}.}
#'     \item{index}{The index page that has the link to the post.
#'       Exist only if passed in the argument \code{index}.}
#'   }
#'
#' @examples
#' url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#'
#' post_df <- get_post(url)
#' head(post_df)
#'
#' # Access information in the list column: 'comment'
#' head(post_df$comment[[1]])
#'
#' # Get Attributes
#' attributes(post_df)
#' attr(post_df, "base_url")
#' attr(post_df, "board")
#'
#' @importFrom dplyr %>% bind_cols
#' @importFrom tibble data_frame as_data_frame
#' @export
get_post <- function(post_url, index = NULL,
                     board_col = FALSE) {

  post_xml <- read_html2(post_url)

  post_meta <- get_post_meta(post_xml,
                             board_col = board_col)
  post_content <- get_post_content(post_xml) # df with 1 row
  post_comment <- get_post_comment(post_xml) # df with many rows
  n_comment <- nrow(post_comment)
  n_push <- sum(post_comment$tag == "Push")
  n_boo <- sum(post_comment$tag == "Boo")
  comment_meta <- cbind(n_comment, n_push, n_boo) %>%
    as.data.frame()

  post_url <- stringr::str_remove(post_url,
                                  "^https://www.ptt.cc/bbs/")

  post_df <- bind_cols(post_meta, post_content,
                       comment = NULL, comment_meta,
                       link = post_url)
  post_df <- as.data.frame(post_df)

  if (is.numeric(index)) { # Add index num
    post_df$index <- index
  }
  post_df$comment[[1]] <- post_comment

  attr(post_df, "base_url") <- "https://www.ptt.cc/bbs/"
  attr(post_df, "board") <- attr(post_meta, "board")

  return(post_df)
}



#' Retrieve mata data from an individual PTT post
#'
#' \code{get_post_meta} returns a data frame with 1 row
#' and 4 cols.
#'
#' @param post_xml \code{xml_document} created by
#'   \code{\link{read_html2}} or \code{\link[xml2]{read_html}}.
#'   See \code{\link[xml2]{read_html}} for details.
#' @param board_col Logical. Whether to set board name as a
#'   variable. Defaults to \code{FALSE}. Note you can get the
#'   board name with \code{attributes{df}$board} or
#'   \code{attr(df, "board")} regardless of the value of this
#'   argument.
#'
#' @return A data frame with 1 row and 4 variables:
#'   \describe{
#'     \item{author}{Author of the post.}
#'     \item{category}{Category of the post, such as
#'       \emph{\enc{新聞}{xin wen}},
#'       \emph{\enc{問卦}{wen gua}}, \emph{Re:}.}
#'     \item{title}{Title of the post.}
#'     \item{date}{The date of the post.}
#'   }
#'
#' @examples
#'
#' #url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#'
#' #post_meta <- get_post_meta(read_html2(url))
#' #post_meta
#'
#' #attributes(post_meta)$board
#'
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom dplyr %>% bind_cols
#' @importFrom tidyr separate
#' @importFrom stringr str_match str_remove str_replace_all
#' @importFrom tibble data_frame as_data_frame
#' @keywords internal
get_post_meta <- function(post_xml, board_col = FALSE) {

  post_meta <- post_xml %>%
    html_nodes("div.article-metaline") %>%
    html_text()

  post_author <- post_meta[1] %>%
    str_remove("^\u4f5c\u8005") #"作者"
  post_cat <- extr_post_category(post_meta[2])
  post_title <- post_meta[2] %>%
    str_remove("^\u6a19\u984c") # "標題"

  post_date <- parse_post_date(post_xml)

  tag <- "div.article-metaline-right > span.article-meta-value"
  post_board <- post_xml %>%
    html_nodes(tag) %>% html_text()

  post_meta <- bind_cols(author = post_author,
                         category = post_cat,
                         title = post_title,
                         date = post_date)

  if (board_col == T) {
    post_meta <- bind_cols(board = post_board,
                           post_meta)
  }

  attr(post_meta, "board") <- post_board

  return(post_meta)
}



#' Retrieve content from an individual PTT post
#'
#' \code{get_post_content} returns a data frame with 1 row and
#' 1 col.
#'
#' @param post_xml \code{xml_document} created by
#' \code{\link{read_html2}} or \code{\link[xml2]{read_html}}
#' See \code{\link[xml2]{read_html}} for details.
#'
#' @return A data frame with 1 row and 2 col.
#'
#' @examples
#' #url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#' #post <- read_html2(url)
#'
#' #get_post_content(post)
#'
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom dplyr %>% bind_cols
#' @importFrom stringr str_remove
#' @importFrom tibble data_frame as_data_frame
#' @keywords internal
get_post_content <- function(post_xml) {

  post_content <- post_xml %>%
    html_node("div#main-content") %>%
    html_text() %>%
    str_remove("^(\n|.)*([0-9]{2}:){2}[0-9]{2} 20[0-9]{2}(\n)+") %>%
    str_remove("(\n)+--\n\u203b(\n|.)*")

  n_char <- post_content %>%
    str_remove_all("( )") %>%
    str_remove_all("(\n)+") %>% nchar()

  post_content <- data_frame(content = post_content, n_char = n_char)
  return(post_content)
}



#' Retrieve user comments from an individual PTT post
#'
#' \code{get_post_comment} returns a data frame with n
#' rows and 5 cols, where n is the number of comments
#' in the post.
#'
#' @param post_xml \code{xml_document} created by
#' \code{\link{read_html2}} or \code{\link[xml2]{read_html}}
#' See \code{\link[xml2]{read_html}} for details.
#'
#' @return A data frame with n rows and 5 variables:
#'   \describe{
#'     \item{tag}{tag of the comment, can be one of the 3 values:
#'       \code{Push} corresponds to \emph{\enc{推}{tui}},
#'       \code{Boo} corresponds to \emph{\enc{噓}{xu}}, and
#'       \code{Neu} corresponds to \emph{\enc{→}{bu tui bu xu}}
#'       }
#'     \item{user}{ID of the user who left the comment.}
#'     \item{comment}{The content of the comment.}
#'     \item{ip}{ip address of the comment.}
#'     \item{time}{The comment date time.}
#'   }
#'
#' @examples
#'
#' #url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#' #post <- read_html2(url)
#'
#' #get_post_comment(post)
#'
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom dplyr %>% bind_cols mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_match str_remove str_replace_all
#' @importFrom tibble data_frame as_data_frame
#' @keywords internal
get_post_comment <- function(post_xml) {

  push <- post_xml %>% html_nodes("div.push")

  push_tag <- push %>%
    html_nodes("span.push-tag") %>%
    html_text() %>%
    str_replace_all("\u2192 ", "Neu") %>%     # →
    str_replace_all("\u63a8 ", "Push") %>%    # 推
    str_replace_all("\u5653 ", "Boo")         # 噓

  push_user <- push %>%
    html_nodes("span.push-userid") %>%
    html_text()

  push_content <- push %>%
    html_nodes("span.push-content") %>%
    html_text() %>%
    str_remove("^: ")

  push_time <- push %>%
    html_nodes("span.push-ipdatetime") %>%
    html_text() %>%
    as_data_frame() %>%
    separate("value", into = c("ip", "date"), sep = -12)

  push_time$ip <- str_remove(push_time$ip, "^ +")
  push_time$ip <- str_remove(push_time$ip, " +$")
  push_time$date <- str_remove(push_time$date, "\n$")

  push_df <- bind_cols(tag = push_tag,
                       user = push_user,
                       comment = push_content,
                       ip = push_time$ip,
                       time = push_time$date)

  post_date <- parse_post_date(post_xml) %>%
    ymd_hms(tz = "Asia/Taipei")

  push_df <- parse_comment_date(push_df, post_date)

  return(push_df)
}
