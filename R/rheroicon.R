#' rheroicon
#'
#' Render an rheroicon icon by name. Use `launch_gallery` to view the icons
#' available in this package or visit the gallery on shinyapps.io (see link
#' below). All icons are rendered with three CSS classes: global ("rheroicons"),
#' icon style ("rheroicons_outline" or "rheroicons_solid"), and icon name (
#' "rheroicons_academic_cap"). Use any of these classes to customize the style
#' of the icon or define your own classes using the argument `class`.
#'
#' @param name an icon name
#' @param type choose icon style "solid" or "outline" (default: "outline")
#' @param classnames a string containing one or more CSS classes
#'
#' @examples
#' rheroicon(name = "academic_cap")
#' rheroicon(name = "academic_cap", type = "solid")
#' rheroicon(name = "academic_cap", classnames = "education-icons")
#'
#'
#' @references
#' \url{https://github.com/tailwindlabs/heroicons}
#' \url{https://davidruvolo.shinyapps.io/rheroicons-demo/}
#'
#' @export
rheroicon <- function(name, type = "outline", classnames = NULL)  {

    icon <- rheroicons[[name]]

    # warn if icon does not exist
    if (is.null(icon)) {
        cli::cli_alert_danger("Icon {.val {name}} cannot be found.")
    }

    # throw error if input value for "type" is invalid
    if (!type %in% c("outline", "solid")) {
        cli::cli_alert_danger("Icon type {.val {type}} is invalid. Use {.val outline} or {.val solid}")
    }

    # process only if icon exists
    if (length(icon) && type %in% c("outline", "solid")) {
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