


















read_html2 <- function(url, ...) {
  if (!grepl("^http", url)) return(read_html(url, encoding = "utf-8", ...))
  curl_1 <- getCurlHandle()
  curlSetOpt(cookie = "over18=1",
             followlocation = TRUE,
             curl = curl_1)
 url <- getURL(url, curl = curl_1)
 read_html(url, encoding = "utf-8", ...)
}














down_html <- function(url, dir, gz = T) {
  curl <- getCurlHandle()
  curlSetOpt(cookie = "over18=1",
             followlocation = TRUE,
             curl = curl)

  for (i in seq_along(url)) {
    raw <- getURL(url[i], curl = curl)
    file <- file.path(dir, basename(url[i]))
    write(raw, file)
  }

  if (gz == T) {
    file <- file.path(dir, list.files(dir))
    for (i in seq_along(file)) {
      gzip(file[i])
    }
  }
}
