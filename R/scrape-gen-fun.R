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
 curl_1 <- getCurlHandle()
 curlSetOpt(cookie = "over18=1",
                   followlocation = TRUE,
                   curl = curl_1)
 url2 <- getURL(url, curl = curl_1)
 read_html(url2, ...)
}
