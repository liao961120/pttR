#' Read PTT Pages with "over18-confirmation"
#'
#' A wrapper of \code{xml2::read_html()} with cookie set
#' to bypass PTT's "over18-confirmation".
#'
#' @param url The url of the target web page.
#'
#' @return An XML document. See \code{\link[xml2]{read_html}}
#'   for more information.
#'
#' @examples
#' url <- "https://www.ptt.cc/bbs/Gossiping/index.html"
#' read_html2(url)
#' @export
read_html2 <- function(url) {
 curl_1 <- RCurl::getCurlHandle()
 RCurl::curlSetOpt(cookie = "over18=1",
                   followlocation = TRUE,
                   curl = curl_1)
 url2 <- RCurl::getURL(url, curl = curl_1)
 xml2::read_html(url2)
}


#' Retrieve Mata Data from an Individual PTT Post
#'
#' \code{get_post_meta} returns a data frame with 1 row and 4 cols.
#'
#' @param post_xml An \code{xml_document} created by
#' \code{\link{read_html2}} or \code{\link[xml2]{read_html}}
#' See \code{\link[xml2]{read_html}} for details.
#'
#' @return A data frame with 1 row and 4 variables:
#'   \describe{
#'     \item{author}{Author of the post.}
#'     \item{category}{Category of the post, such as
#'       \emph{新聞}, \emph{問卦}, \emph{Re:}. See
#'       \code{\link{extr_post_category}} for category extration.}
#'     \item{title}{Title of the post.}
#'     \item{date}{The date of the post.}
#'   }
#'
#' @examples
#'
#' url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#' post <- read_html2(url)
#'
#' get_post_meta(post)
#'
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom dplyr %>% bind_cols
#' @importFrom tidyr separate
#' @importFrom stringr str_match str_remove str_replace_all
#' @importFrom tibble data_frame as_data_frame
#' @export
get_post_meta <- function(post_xml) {

  post_meta <- post_xml %>%
    html_nodes("div.article-metaline") %>%
    html_text()

  post_author <- post_meta[1] %>%
    str_remove("^\u4f5c\u8005") #"作者"
  post_cat <- extr_post_category(post_meta[2])
  post_title <- post_meta[2] %>%
    str_remove("^\u6a19\u984c") # "標題"

  ## DateTime
  mon <- paste0(month.abb, collapse = "|")
  mon <- paste0("(", mon, ") ",
                paste0("[ 0-9][0-9] ([0-9]{2}:){2}[0-9]{2} 20[0-9]{2}"))
  dum <- post_meta[3] %>% str_match(mon)

  post_date <- dum[1, 1] %>%
    strptime("%b %e %H:%M:%S %Y", tz = "CST") %>%
    as.character()

  post_meta <- bind_cols(author = post_author,
                         category = post_cat,
                         title = post_title,
                         date = post_date)
  return(post_meta)
}



#' Retrieve Post Content from an Individual PTT Post
#'
#' \code{get_post_content} returns a data frame with 1 row and 1 col.
#'
#' @param post_xml An \code{xml_document} created by
#' \code{\link{read_html2}} or \code{\link[xml2]{read_html}}
#' See \code{\link[xml2]{read_html}} for details.
#'
#' @return A data frame with 1 row and 1 col. The entry is a long
#'   string.
#'
#' @examples
#'
#' url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#' post <- read_html2(url)
#'
#' get_post_content(post)
#'
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom dplyr %>% bind_cols
#' @importFrom stringr str_remove
#' @importFrom tibble data_frame as_data_frame
#' @export
get_post_content <- function(post_xml) {

  post_content <- post_xml %>%
    html_node("div#main-content") %>%
    html_text() %>%
    # remove head portion
    str_remove("^(\n|.)*([0-9]{2}:){2}[0-9]{2} 20[0-9]{2}(\n)+") %>%
    # remove tail portion
    str_remove("(\n)+--\n\u203b(\n|.)*")

  post_content <- data_frame(content = post_content)
  return(post_content)
}



#' Retrieve User Comments from an Individual PTT Post
#'
#' \code{get_post_comment} returns a data frame with n rows and 5
#' cols, where n is the number of comments in the post.
#'
#' @param post_xml An \code{xml_document} created by
#' \code{\link{read_html2}} or \code{\link[xml2]{read_html}}
#' See \code{\link[xml2]{read_html}} for details.
#'
#' @return A data frame with n rows and 5 variables:
#'   \describe{
#'     \item{tag}{tag of the comment, can be one of the 3 values:
#'       \code{Push} corresponds to "推",
#'       \code{Boo} corresponds to "噓", and
#'       \code{Neu} corresponds to "→".}
#'     \item{user}{ID of the user who left the comment.}
#'     \item{comment}{The content of the comment.}
#'     \item{ip}{ip address of the comment.}
#'     \item{time}{The comment date time.}
#'   }
#'
#' @examples
#'
#' url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#' post <- read_html2(url)
#'
#' get_post_comment(post)
#'
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom dplyr %>% bind_cols mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_match str_remove str_replace_all
#' @importFrom tibble data_frame as_data_frame
#' @export
get_post_comment <- function(post_xml) {
  # post_xml is object returned by xml2::read_html()
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
    separate("value", into = c("ip", "date"), sep = 16)

  push_time <- push_time %>%
    mutate(ip = str_remove(ip, "^ +")) %>%
    mutate(ip = str_remove(ip, " +$")) %>%
    mutate(date = str_remove(date, "\n$"))

  push_df <- bind_cols(tag = push_tag,
                       user = push_user,
                       comment = push_content,
                       ip = push_time$ip,
                       time = push_time$date)
  return(push_df)
}



#' Get Information from an Individual PTT Post
#'
#' \code{get_post} returns a data frame with 1 row and 9 cols,
#' where the column \code{comment} is a list column.
#'
#' This is a function that combines the data gathered from
#' three functions: \code{\link{get_post_meta}},
#' \code{\link{get_post_content}}, and
#' \code{\link{get_post_comment}}.
#'
#' @param post_xml An \code{xml_document} created by
#' \code{\link{read_html2}} or \code{\link[xml2]{read_html}}
#' See \code{\link[xml2]{read_html}} for details.
#'
#' @return A data frame with 1 row and 9 variables:
#'   \describe{
#'     \item{author}{Author of the post.}
#'     \item{category}{Category of the post, such as
#'       \emph{新聞}, \emph{問卦}, \emph{Re:}. See
#'       \code{\link{extr_post_category}} for category extration.}
#'     \item{title}{Title of the post.}
#'     \item{date}{The date of the post.}
#'     \item{content}{The content of the post.}
#'     \item{comment}{A list column.
#'       See \code{\link{get_post_comment}} for information about
#'       entries in this list column.}
#'     \item{n_comment}{Number of comments.}
#'     \item{n_push}{Number of "Push" comments.}
#'     \item{n_boo}{Number of "Boo" comments.}
#'   }
#'
#' @examples
#'
#' url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#' post <- read_html2(url)
#'
#' post_df <- get_post(post)
#'
#' # Access information in the list column: 'comment'
#' post_df$comment[[1]]
#'
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom dplyr %>% bind_cols
#' @importFrom tidyr separate
#' @importFrom stringr str_match str_remove str_replace_all
#' @importFrom tibble data_frame as_data_frame
#' @export
get_post <- function(post_xml) {

  post_meta <- get_post_meta(post_xml)  # df with 1 row
  post_content <- get_post_content(post_xml) # df with 1 row
  post_comment <- get_post_comment(post_xml) # df with many rows
  n_comment <- nrow(post_comment)
  n_push <- sum(post_comment$tag == "Push")
  n_boo <- sum(post_comment$tag == "Boo")
  comment_meta <- cbind(n_comment, n_push, n_boo) %>%
    as_data_frame()

  post_df <- bind_cols(post_meta, post_content,
                       comment = NULL, comment_meta)
  post_df$comment[[1]] <- post_comment

  warning("Ignore \"Unknown or uninitialised column: 'comment'\". It is a bug in tibble.")

  return(post_df)
}


