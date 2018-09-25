#' @rdname segment
#' @export
seg_content <- function(x, words = NULL, tags = NULL, user = NULL) {
  # alias of seg_ptt
  seg_ptt(x, words = words, tags = tags, user = user)
}

#' @rdname segment
#' @export
seg_comment <- function(x, words = NULL, tags = NULL, user = NULL) {
  # x is a list column with df in every element
  for (i in seq_along(x)) {
    if (is.null(x[[i]])) break
    df <- x[[i]]
    df$comment <- seg_ptt(df$comment, words = words,
                          tags = tags, user = user)
    x[[i]] <- df
  }
  return(x)
}



#' Word segmentation for PTT post content
#'
#' @param x Character vector. Text to be segmented.
#' @importFrom jiebaR worker new_user_word segment
#' @keywords internal
seg_ptt <- function(x, words = NULL, tags = NULL, user = NULL) {

  # Set up jiebaR seg function
  if (is.null(user)) user <- system.file('pttdict/pttdict.csv', package = 'pttR')
  seg <- worker(bylines = T, symbol = T, user = user)
  if (is.character(words)) {
    if (is.null(tags)) tags <- rep('n', length(words))
    new_user_word(seg, words = words, tags = tags)
  }
  # Main function
  segged <- lapply(segment(x, seg),
                   function(x) paste(x, collapse = ' '))
  return(unlist(segged))
}



#### Common Documentation ####

#' Word segmentation for PTT post content and comments.
#'
#' @param x Column 'content' or 'comment' from a data frame
#'   returned by \code{post2df}.
#' @param words Character vector. A vector of words to
#'   pass to jiebaR dictionary.
#'   See \code{\link[jiebaR]{new_user_word}} for details.
#' @param tags Character vector. A vector of tags
#'   specifying the lexical categories of the words in
#'   `words`. Defaults to `n` (noun).
#'   See \code{\link[jiebaR]{new_user_word}} for details.
#' @param user Character. A string specifying the path to
#'   an user defined dictionary. Defaults to pttR built-in
#'   dictionary. See
#'   \href{jiebaR doc}{https://qinwenfeng.com/jiebaR/worker-.html#user-}
#'   for details.
#' @source
#'   For details about the built-in ptt dictionary,
#'   see \url{https://liao961120.github.io/PTT-scrapy/}.
#' @name segment
NULL
