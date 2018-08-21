#' Open web page in default browser
#'
#' @importFrom utils browseURL
#' @keywords internal
browse <- function(URL = "https://liao961120.github.io/pttR/") {
  cat("Visiting ", URL, "...\n")
  Sys.sleep(1)
  cat("Press 'Esc' to stop.\n")
  Sys.sleep(1)
  cat("Opening Browser in 3\n")
  Sys.sleep(1)
  for (i in 2:1) {
    cat("                  ", i,"\n")
    Sys.sleep(1)
  }
  browseURL(URL)
}
