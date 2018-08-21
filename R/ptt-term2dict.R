# Add new term to internal PTT dictionary
# expand_dict <- function(x) {
#   print(pryr::where("ptt_dict"))
#   x <- c(x, "self-defined")
#   ptt_dict <- rbind(ptt_dict, x)
# }

#' Get PTT dictionary
#'
#' @export
get_ptt_dict <- function() {
  return(ptt_dict)
}
