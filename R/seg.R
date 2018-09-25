
## idx_df <- index2df("gossiping",
##                    search_term = '林飛帆',
##                    search_page = 1:5)

## pttR::down_html(as_url(idx_df$link), dir = "local/fan/")

#library(pttR)
#post_df <- post2df(list.files('local/fan', full.names = T))


#' Word segmentation for PTT post content
#'
#' @param x Character vector or data frame. Text to be
#'   segmented. If a data frame is passed, the column
#'   'content' or 'comment' will be extracted and
#'   use as the input character vector.
#' @param words Character vector. A vector of words to
#'   pass to jiebaR dictionary.
#'   See \code{\link[jiebaR]{new_user_word}} for details.
#' @param tags Character vector. A vector of tags
#'   specifying the lexical categories of the words in
#'   `words`. Defaults to `n` (noun).
#'   See \code{\link[jiebaR]{new_user_word}} for details.
#' @param user Character. A string specifying the path to
#'   user defined dictionary. Defaults to pttR built-in
#'   dictionary. See
#'   \href{jiebaR doc}{https://qinwenfeng.com/jiebaR/worker-.html#user-} for details.
#' @source \url{https://liao961120.github.io/PTT-scrapy/}
#' @importFrom jiebaR worker new_user_word segment
seg_ptt <- function(x, words = NULL, tags = NULL, user = NULL) {
  stopifnot((is.character(x) || is.data.frame(x)))

  # Set up jiebaR seg function
  if (is.null(user)) user <- system.file('pttdict/pttdict.csv', package = 'pttR')
  seg <- worker(bylines = T, symbol = T, user = user)
  if (is.character(words)) {
    if (is.null(tags)) tags <- rep('n', length(words))
    new_user_word(seg, words = words, tags = tags)
  }
  # Main function
  if (is.data.frame(x)) {
    if ('content' %in% colnames(x)) {
      x <- x$content
    } else if ('comment' %in% colnames(x)) {
      x <- x$comment
    } else {
      stop('Invalid input data frame.
           Must contain column "content" or "comment".')
    }
  }
  segged <- lapply(segment(x, seg),
                   function(x) paste(x, collapse = ' ')
                  )
  return(unlist(segged))
}



