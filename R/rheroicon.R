#' An inline SVG icon from Heroicons
#'
#' Render an icon by name. Run 'launch_gallery' to view all available icons
#' or search for icons using the 'find_icons' function.
#'
#' @param name an icon name
#' @param type render a 'solid' or 'outline' icon (default: 'outline')
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
#'      'type'. Icons can have 'rheroicons_outline' of 'rheroicons_solid'.
#'   }
#'   \item{icon name}{
#'     The icon name is also passed into the list of 'CSS' class. These
#'     are always structured in the following format: 'rheroicons_icon'.
#'     If the icon is 'thumb_down', the 'CSS' class would be
#'     'rheroicons_thumb_down'.
#'   }
#' }
#'
#' Use any of combination of these classes to style icons via 'CSS'.
#' In addition, you apply your own 'CSS' classes using the 'class' argument.
#'
#' @examples
#' rheroicon(name = "academic_cap")
#' rheroicon(name = "academic_cap", type = "solid")
#' rheroicon(name = "academic_cap", class = "education-icons")
#'
#' @return An string containing the 'SVG' markup of an icon
#'
#' @references
#' \url{https://heroicons.com}
#' \url{https://github.com/tailwindlabs/heroicons}
#'
#' @export
rheroicon <- function(name, type = "outline", class = NULL)  {

    icon <- rheroicons[[name]]

    # warn if icon does not exist
    if (is.null(icon)) {
        warning("Icon does not exist")
    }

    # throw error if input value for "type" is invalid
    valid_types <- c("outline", "solid")
    if (!type %in% valid_types) {
        warning("Icon type is invalid. Use 'outline' or 'solid'")
    }

    # process only if icon exists
    if (length(icon) && (type %in% valid_types)) {
        svg <- icon$icons[[type]]

        # append class attribute if applicable
        if (!is.null(class)) {
            svg <- .set__classnames(
                svg = svg,
                class = class
            )
        }

        # return element as html
        htmltools::HTML(svg)
    }

}