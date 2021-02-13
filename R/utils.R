#' Process CSS Classes
#'
#' In the main icon function, the optional argument 'class' can be
#' used add custom CSS classes to an icon.
#'
#' @param svg a string containing the SVG markup of an icon
#' @param class a string containing one or more CSS classes
#'
#' @noRd
.set__classnames <- function(svg, class) {

    # extract SVG's css classes
    css <- stringr::str_extract(
        string = svg,
        pattern = "class=.([\\w\\s]+)."
    )

    # extract positions of \"
    css_pos <- stringr::str_locate_all(
        string = css,
        pattern = "\""
    )[[1]]

    # define new css
    new_css <- paste0(
        stringr::str_sub(css, start = 1, end = css_pos[2, 2] - 1),
        " ",
        class,
        "\""
    )

    # update
    stringr::str_replace(
        string = svg,
        pattern = css,
        replacement = new_css
    )
}