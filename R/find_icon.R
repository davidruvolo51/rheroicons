#' Find available icons
#'
#' @param query a search term passed down to \code{stringr::str_subset}.
#'      Alternatively, leave blank to view all icon names.
#'
#' @examples
#' find_icons(query = "chevron")
#' find_icons(query = "chevron|arrow")
#' find_icons(query = "down|up|left|right")
#'
#' @return An array of icon names
#'
#' @references
#' \url{https://heroicons.com}
#' \url{https://github.com/tailwindlabs/heroicons}
#' 
#' @export
find_icons <- function(query = "") {
    stringr::str_subset(string = names(rheroicons), pattern = query)
}