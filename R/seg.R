

seg_content <- function(x, words = NULL, tags = NULL, user = NULL) {

  seg_ptt(x, words = words, tags = tags, user = user)
}



seg_comment <- function(x, words = NULL, tags = NULL, user = NULL) {

  for (i in seq_along(x)) {
    if (is.null(x[[i]])) break
    df <- x[[i]]
    df$comment <- seg_ptt(df$comment, words = words,
                          tags = tags, user = user)
    x[[i]] <- df
  }
  return(x)
}








seg_ptt <- function(x, words = NULL, tags = NULL, user = NULL) {


  if (is.null(user)) user <- system.file('pttdict/pttdict.csv', package = 'pttR')
  seg <- worker(bylines = T, symbol = T, user = user)
  if (is.character(words)) {
    if (is.null(tags)) tags <- rep('n', length(words))
    new_user_word(seg, words = words, tags = tags)
  }

  segged <- lapply(segment(x, seg),
                   function(x) paste(x, collapse = ' '))
  return(unlist(segged))
}

























NULL
