#' A dictionary for Ptt
#'
#' A dataset containing Ptt-specific terms scraped from
#' \href{http://zh.pttpedia.wikia.com}{pttpedia}.
#'
#' @format A data frame with 2 variables
#' \describe{
#'   \item{term}{Ptt-specific terms}
#'   \item{source}{wiki pages where the terms are scraped
#'    \itemize{
#'      \item{\href{https://bit.ly/2nEfrQF}{board_name}}
#'      \item{\href{https://bit.ly/2OBli4y}{event_name}}
#'      \item{\href{https://bit.ly/2MeDG6E}{famous_people}}
#'      \item{\href{https://bit.ly/25ub22j}{culture}}
#'      \item{\href{https://bit.ly/2KTAzvx}{notation}}
#'      \item{\href{https://bit.ly/2KPHTZ6}{basic_term}}
#'      \item{\href{https://bit.ly/2BdKQmR}{post_type}}
#'      \item{\href{https://bit.ly/2w6oQnX}{newest}}
#'    }
#'   }
#' }
#' @source \url{http://zh.pttpedia.wikia.com}
#' @examples
#' head(ptt_dict)
#'
#' # get the date of the newest page
#' attr(ptt_dict, "date")
"ptt_dict"
