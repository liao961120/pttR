#' Get all information from an individual PTT post
#'
#' \code{get_post} returns a data frame with 1 row and 9 cols,
#' where the column \code{comment} is a list column.
#'
#' @param post_url Character. An URL of a PTT post.
#' @param board_col Logical. Whether to set board name as a
#'   variable. Defaults to \code{FALSE}.
#'
#' @return A data frame with 1 row and 10 variables:
#'   \describe{
#'     \item{author}{Author of the post.}
#'     \item{category}{Category of the post, such as
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{xin wen}}",
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{wen gua}}",
#'       "\emph{Re:}".}
#'     \item{title}{Title of the post.}
#'     \item{date}{The date of the post.}
#'     \item{content}{The content of the post.}
#'     \item{comment}{A list column.
#'       See \code{\link{get_post_comment}} for information
#'       about entries in this list column.}
#'     \item{n_comment}{Number of comments.}
#'     \item{n_push}{Number of "Push" comments.}
#'     \item{n_boo}{Number of "Boo" comments.}
#'     \item{link}{URL of the post with
#'       \url{https://www.ptt.cc/bbs/} removed.}
#'   }
#'   One additional variable is optional:
#'   \describe{
#'     \item{board}{The board the post belongs to. Exist only
#'       if \code{board_col = TRUE}.}
#'   }
#'
#' @import rvest
#' @importFrom dplyr bind_cols
#' @importFrom stringr str_detect str_match str_remove str_remove_all
#' @importFrom tibble data_frame as_data_frame
#' @keywords internal
get_post <- function(post_url, board_col = FALSE) {

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

  if (str_detect(post_url, "^http")) {
    post_url <- str_remove(post_url, "^https://www.ptt.cc/bbs/")
  } else {
    post_url <- basename(post_url)
  }

  post_df <- bind_cols(post_meta, post_content,
                       comment = NULL, comment_meta,
                       link = post_url)
  post_df <- as.data.frame(post_df)

  post_df$comment[[1]] <- post_comment

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
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{xin wen}}",
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{wen gua}}",
#'       "\emph{Re:}".}
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
    str_remove("^\u4f5c\u8005")  #"作者"
  post_cat <- extr_post_category(post_meta[2])
  post_title <- post_meta[2] %>%
    str_remove("^\u6a19\u984c")  # "標題"

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
#' @importFrom stringr str_remove str_remove_all
#' @importFrom tibble data_frame as_data_frame
#' @keywords internal
get_post_content <- function(post_xml) {

  post_content <- post_xml %>%
    html_node("div#main-content") %>%
    html_text() %>%
    str_remove("^(\n|.)*([0-9]{2}:){2}[0-9]{2} 20[0-9]{2}(\n)+") %>%
    str_remove("(\n)+--\n\u203b(\n|.)*")

  post_content <- data_frame(content = post_content)
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
#'       \code{Push} corresponds to
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{tui}}",
#'       \code{Boo} corresponds to
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{xu}}", and
#'       \code{Neu} corresponds to
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{bu tui bu xu}}"
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
