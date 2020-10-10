#' Find Icons
#'
#' Find an icon using a search term
#'
#' @param query a search term passed down to \code{stringr::str_subset}
#'
#' @examples
#' find_icons(query = "chevron")
#' find_icons(query = "chevron|arrow")
#' find_icons(query = "down|up|left|right")
#'
#' @return Find an icon using a search term
#'
#' @export
find_icons <- function(query = "") {
    stringr::str_subset(
        string = names(rheroicons),
        pattern = query
    )
}