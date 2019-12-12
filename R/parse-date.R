
























parse_comment_date <- function(cmt_df, post_date) {

  df <- cmt_df
  year <- year(post_date)
  df$ymd <- mmdd2date(df$time, year)



  first_cmt_fl <- floor_date(df$ymd[1], "minute") + seconds(1)
  post_date_fl <- floor_date(post_date, "minute")
  if (first_cmt_fl < post_date_fl) {
    year <- year + 1
    df$ymd <- mmdd2date(df$time, year)
  }


  df$ymd_cp <- append(NA, as.character(df$ymd))[1:nrow(df)]
  df$ymd_cp <- ymd_hms(df$ymd_cp, tz = "Asia/Taipei")




  idx <- which(df$ymd < df$ymd_cp)



  df <- dplyr::mutate(df, year = year)
  for (i in idx) {
    df$year[i:nrow(df)] <- df$year[i:nrow(df)] + 1
  }

  df$time <- paste(df$year, df$time, sep = "/") %>%
    ymd_hm(tz = "Asia/Taipei")

  return(dplyr::select(df, colnames(cmt_df)))
}







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



mmdd2date <- function(md_hm, year) {
  paste(year, md_hm, sep = "/") %>%
    ymd_hm(tz = "Asia/Taipei")
}
