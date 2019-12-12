#' Read PTT pages with "over18-confirmation"
#'
#' A wrapper of \code{xml2::read_html()} with cookie set
#' to bypass PTT's "over18-confirmation".
#'
#' @param url Character. The URL of the target web page.
#' @param ... Additional arguments passed on to
#'   \code{\link[xml2]{read_html}}.
#'
#' @return An XML document. See \code{\link[xml2]{read_html}}
#'   for more information.
#'
#' @examples
#' url <- "https://www.ptt.cc/bbs/Gossiping/index.html"
#' read_html2(url)
#'
#' @importFrom RCurl getCurlHandle curlSetOpt getURL
#' @importFrom xml2 read_html
#' @export
read_html2 <- function(url, ...) {
  if (!grepl("^http", url)) return(read_html(url, encoding = "utf-8", ...))
  curl_1 <- getCurlHandle()
  curlSetOpt(cookie = "over18=1",
             followlocation = TRUE,
             curl = curl_1)
 url <- getURL(url, curl = curl_1)
 read_html(url, encoding = "utf-8", ...)
}

#' Dowload HTML files to local directory
#'
#' @param url Character vector. A list of URLs of posts to
#'   download.
#' @param dir Character. Path to the directory to store
#'   dowloaded files.
#' @param gz Logical. Whether to compress the downloaded
#'   files. Defaults to \code{TRUE}. Uses gzip compression.
#'
#' @importFrom RCurl getCurlHandle curlSetOpt getURL
#' @importFrom xml2 read_html
#' @importFrom R.utils gzip
#' @export
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
