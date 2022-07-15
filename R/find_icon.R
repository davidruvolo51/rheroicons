#' Find available icons
#'
#' You can search for icons by querying the icon set. For example, if you would
#' like to find icons that are arrows and chevrons, then type in you query as a
#' regexp: 'arrows|chevrons'. Alternatively, if you would like to view all icon
#' names, leave the value for 'query' blank.
#'
#' @param query a string containing a search term
#'
#' @examples
#' find_icons(query = "chevron")
#' find_icons(query = "chevron|arrow")
#' find_icons(query = "down|up|left|right")
#' find_icons(query = "_circle_")
#' find_icons(query = "(\\_down)$")
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