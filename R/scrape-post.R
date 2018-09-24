#' Extract information from PTT posts
#'
#' \code{post2df} takes paths to PTT posts (either URLs or local
#' path to HTML files) as input, extracts information from the
#' posts, and returns a data frame with n row and 12 cols with
#' one post per row.
#'
#' This is a function that rbinds the data gathered from
#' \code{\link{get_post}}, and add some meta data about the
#' 'content' of the post.
#'
#' @param path Character Vector. A vector of URLs or local paths
#'   to PTT posts.
#' @param board_col Logical. Whether to set board name as a new
#'   variable. Defaults to \code{FALSE}.
#'
#' @return A data frame with n rows and 12 variables:
#'   \describe{
#'     \item{author}{Author of the post.}
#'     \item{category}{Category of the post, such as
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{xin wen}}",
#'       "\emph{\href{https://liao961120.github.io/pttR/articles/translation}{wen gua}}",
#'       "\emph{Re:}".}
#'     \item{title}{Title of the post.}
#'     \item{date}{The date of the post.}
#'     \item{content}{The content of the post.}
#'     \item{content_char}{The Number of characters in the post
#'       content. Whitespaces and newline characters are
#'       removed before counting.}
#'     \item{n_comment}{Number of comments.}
#'     \item{n_push}{Number of "Push" comments.}
#'     \item{n_boo}{Number of "Boo" comments.}
#'     \item{link}{URL of the post with
#'       \url{https://www.ptt.cc/bbs/} removed.
#'       For local file paths, the link is the file name.}
#'     \item{comment}{A list-column with data frames stored
#'       inside. Contents extracted from the post comment
#'       region. See \code{\link{get_post_comment}} for
#'       information about the variables in the data frame.}
#'     \item{comment_urls}{A list-column with character vectors
#'       stored inside. URLs extracted from post content are stored
#'       inside the character vectors. The original URLs in the
#'       post content are replaced as '\emph{rm_URL}' in the
#'       variable 'content'.}
#'   }
#'   One additional variable is optional:
#'   \describe{
#'     \item{board}{The board the post belongs to. Exist only
#'       if \code{board_col = TRUE}.}
#'    }
#'
#' @examples
#' url <- "https://www.ptt.cc/bbs/Gossiping/M.1534415307.A.BE5.html"
#'
#' post_df <- post2df(url)
#' head(post_df)
#'
#' # Access information in the list column: 'comment'
#' head(post_df$comment[[1]])
#'
#' \dontrun{
#' # Read from local files
#' post_df <- post2df(list.files('local/gossiping', full.names = T))
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @export
post2df <- function(path, board_col = FALSE) {

  # Generate df with nrow = length(path)
  posts_df <- vector("list", length = length(path))
  for (i in seq_along(path)) {
    posts_df[[i]] <- get_post(path[i])
  }
  posts_df <- bind_rows(posts_df)

  # Extract information from the column 'content'
  idx <- which(colnames(posts_df) == "content")

  posts_df <- mutate_content_len(posts_df, idx) %>%
     mutate_content_url(idx)

  return(posts_df)
}


### Internal Helpers ---------

#' Word count 'content' col of get_post
#' @importFrom magrittr %>%
#' @importFrom stringr str_remove_all
#' @keywords internal
mutate_content_len <- function(df, idx) {
  df$content_char <- df[, idx] %>%
    str_remove_all("( )") %>%
    str_remove_all("(\n)+") %>% nchar()
  return(df)
}

#' Extract and remove URL from 'content' column of a data frame returned by get_post()
#' @importFrom stringr str_extract_all str_replace_all
#' @keywords internal
mutate_content_url <- function(df, idx, rpl = "rm_URL") {
  reg_url <- "(http[^ ]*)|(www\\.[^ ]*)"

  df$content_urls <- str_extract_all(df[, idx], reg_url)
  df[, idx] <- str_replace_all(df[, idx], reg_url, "rm_URL")

  return(df)
}




