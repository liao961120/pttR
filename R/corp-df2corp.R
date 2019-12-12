



















NULL
















post2qcorp <- function(df, ...) {
  df <- df[, -which(colnames(df) == "comment")]
  post_corp <- corpus(df,
                 docid_field = "title",
                 text_field = "content", ...)
}

















post2tmcorp <- function(df, PCorpus = FALSE, ...) {
  df <- df[, -which(colnames(df) == "comment")]

  df <- rename(df, "doc_id" = "link") %>%
    rename("text" = "content") %>%
    select("doc_id", "text", everything())

  ifelse(PCorpus == T,
         docs <- PCorpus(DataframeSource(df), ...),
         docs <- VCorpus(DataframeSource(df), ...))
  return(docs)
}






















NULL











comment2qcorp <- function(df) {
  comment_corp <- vector("list", length = nrow(df))

  for (i in seq_along(df$comment)) {
    if (is.null(df$comment[[i]])) {
      comment_corp[[i]] <- df$comment[[i]]
    } else {
      comment_corp[[i]] <- corpus(df$comment[[i]],
                                  text_field = "comment",
                                  docid_field = "time")
    }
  }
  df <- data_frame(post_id = df$link,
                   comment = comment_corp)
}














comment2tmcorp <- function(df, PCorpus = FALSE, ...) {

  if (PCorpus == T) {
    corp <- function(df, ...) PCorpus(DataframeSource(df), ...)
  } else {
    corp <- function(df, ...) VCorpus(DataframeSource(df), ...)
  }

  as_source <- function(df) {
    df <- df %>%
      mutate("doc_id" = 1:nrow(df)) %>%
      rename("text" = "comment") %>%
      select("doc_id", "text", everything())
  }

  comment_corp <- vector("list", length = nrow(df))
  for (i in seq_along(df$comment)) {
    if (is.null(df$comment[[i]])) {
      comment_corp[[i]] <- df$comment[[i]]
    } else {
      comment_corp[[i]] <- corp(as_source(df$comment[[i]]), ...)
    }
  }
  df <- data_frame(post_id = df$link,
                   comment = comment_corp)
}
