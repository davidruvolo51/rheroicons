#' An inline SVG icon from heriocons
#'
#' Render an rheroicon icon by name. Use `launch_gallery` to view the icons
#' available in this package or visit the heroicons icon gallery
#' (see referenced urls).
#'
#' @param name a heroicon icon name
#' @param type render a "solid" or "outline" icon (default: "outline")
#' @param classnames a string containing one or more CSS classes (optional)
#'
#' @section Styling Icons with classnames:
#'
#' Icons are rendered from svg strings. All icons have three css classes
#' defined: global ("rheroicons"), icon style ("rheroicons_outline" or
#' "rheroicons_solid"), and icon name ("rheroicons_academic_cap").
#' Use any of combination of these classes to style icons via css.
#' Alternatively, you can pass your own classes using the \code{classnames}
#' argument.
#'
#' @examples
#' rheroicon(name = "academic_cap")
#' rheroicon(name = "academic_cap", type = "solid")
#' rheroicon(name = "academic_cap", classnames = "education-icons")
#'
#' @return Returns an svg string of an Heroicon icon
#'
#' @references
#' \url{https://github.com/tailwindlabs/heroicons}
#' \url{https://heroicons.com}
#'
#' @export
rheroicon <- function(name, type = "outline", classnames = NULL)  {

    icon <- rheroicons[[name]]

    # warn if icon does not exist
    if (is.null(icon)) {
        cli::cli_alert_danger("Icon {.val {name}} cannot be found.")
    }

    # throw error if input value for "type" is invalid
    valid_types <- c("outline", "solid")
    if (!type %in% valid_types) {
        cli::cli_alert_danger(
            "Type {.val {type}} is invalid. Use {.val {.valid_types}}"
        )
    }

    # process only if icon exists
    if (length(icon) && (type %in% valid_types)) {
        svg <- icon$icons[[type]]

        # append class attribute if applicable
        if (!is.null(classnames)) {
            svg <- .set__classnames(
                svg = svg,
                class = classnames
            )
        }

        # return element as html
        htmltools::HTML(svg)
    }

}