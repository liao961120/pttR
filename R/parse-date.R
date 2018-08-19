#' Parse dates to add year in PTT post comments
#'
#' To deal with the problem of loss of information,
#' i.e. a date with its year truncated can't be converted
#' back to the original date, two assumptions are made:
#' \enumerate{
#'   \item The time interval between (1) the time the post
#'     was published and (2) the time the first comment
#'     was left is less than 1 year.
#'   \item The time interval between two adjacent comment
#'     is less than 1 year.
#' }
#'
#' @param cmt_df A data frame with format almost identical
#'   to that returned by \code{\link{get_post_comment}},
#'   except that the \code{time} column has entries with
#'   the format of the date in PTT post comments.
#' @param post_date Date. A value returned by
#'   \code{\link{lubridate::ymd_hms}}.
#'
#' @return A data frame with dimension same as the input.
#'
#' @importFrom lubridate year years seconds ymd_hms ymd_hm floor_date
#' @export
#' @keywords internal
parse_comment_date <- function(cmt_df, post_date) {

  df <- cmt_df
  year <- year(post_date)
  df$ymd <- mmdd2date(df$time, year)

  # Deal with rare case where post_date and comment
  # dates have different years
  first_cmt_fl <- floor_date(df$ymd[1], "minute") + seconds(1)
  post_date_fl <- floor_date(post_date, "minute")
  if (first_cmt_fl < post_date_fl) {
    year <- year + 1
    df$ymd <- mmdd2date(df$time, year)
  }

  # Make a col to put 'the next comment' side-by-side
  df$ymd_cp <- append(NA, as.character(df$ymd))[1:nrow(df)]
  df$ymd_cp <- ymd_hms(df$ymd_cp, tz = "Asia/Taipei")

  # ymd_cp should be earlier than ymd
  # find out the reverse case,
  # i.e. comments that cross-year
  idx <- which(df$ymd < df$ymd_cp)

  # For every cross-year comment, add 1 to all entries
  # below. Loop over all cross-year cases.
  df <- dplyr::mutate(df, year = year)
  for (i in idx) {
    df$year[i:nrow(df)] <- df$year[i:nrow(df)] + 1
  }

  df$time <- paste(df$year, df$time, sep = "/") %>%
    ymd_hm(tz = "Asia/Taipei")

  return(dplyr::select(df, colnames(cmt_df)))
}


#' Extract the publish date of a PTT post
#'
#' @param post_xml \code{xml_document}.
#'
#' @keywords internal
parse_post_date <- function(post_xml) {

  post_meta <- post_xml %>%
    html_nodes("div.article-metaline") %>%
    html_text()

  mon <- paste0(month.abb, collapse = "|")
  mon <- paste0("(", mon, ") ",
                paste0("[ 0-9][0-9] ([0-9]{2}:){2}[0-9]{2} 20[0-9]{2}"))
  dum <- post_meta[3] %>% str_match(mon)

  post_date <- dum[1, 1] %>%
    strptime("%b %e %H:%M:%S %Y", tz = "CST") %>%
    as.character()

  return(post_date)
}

# Convert month-day-hm to year-month-day-hms
# quick and dirty script
mmdd2date <- function(md_hm, year) {
  paste(year, md_hm, sep = "/") %>%
    ymd_hm(tz = "Asia/Taipei")
}
