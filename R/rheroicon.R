#' An inline SVG icon from Heroicons
#'
#' Render an icon by name. Run 'launch_gallery' to view all available icons
#' or search for icons using the 'find_icons' function.
#'
#' @param name string containing the name of an icon
#' @param type render icon by style; either 'solid', 'outline', or 'mini' (default: 'outline')
#' @param class a string containing one or more 'CSS' classes (optional)
#'
#' @section Styling Icons with 'CSS' class names:
#'
#' Icons are rendered from 'SVG' strings. All icons have three 'CSS' classes
#' defined: global, icon style, and icon name.
#'
#' \describe{
#'   \item{global}{All icons are returned with the 'CSS' class 'rheroicons'}
#'   \item{icon style}{
#'      All icons have a solid and an outlined version.
#'      The icon style 'CSS' class is determined by the value entered for
#'      'type'. Icons can have 'rheroicons-outline', 'rheroicons-solid', or
#'      'rheroicons-mini'.
#'   }
#'   \item{icon name}{
#'     The icon name is also passed into the list of 'CSS' class. These
#'     are always structured in the following format: 'rheroicons-icon'.
#'     If the icon is 'hand-thumb-down', the 'CSS' class would be
#'     'rheroicons-hand-thumb-down'.
#'   }
#' }
#'
#' Use any of combination of these classes to style icons via 'CSS'.
#' In addition, you apply your own 'CSS' classes using the 'class' argument.
#'
#' @examples
#' rheroicon(name = "face-smile")
#' rheroicon(name = "face-smile", type = "solid")
#' rheroicon(name = "face-smile", class = "my-icon-set")
#'
#' @return An string containing the 'SVG' markup of an icon
#'
#' @references
#' \url{https://heroicons.com}
#' \url{https://github.com/tailwindlabs/heroicons}
#'
#' @export
rheroicon <- function(name=NULL, type = "outline", class = NULL)  {
  stopifnot("Icon NULL does not exist" = !is.null(name))
  
  valid_types <- c("outline", "solid", "mini")
  icon <- rheroicons[[name]]

  if (!type %in% valid_types) {
    warning("Icon type is invalid. Use 'outline','solid', or 'mini'.")
  }

  if (length(icon) && (type %in% valid_types)) {
    svg <- icon[[type]]
    if (!is.null(class)) {
      svg <- .set__classnames(svg = svg, class = class)
    }
    htmltools::HTML(svg)
  }
}