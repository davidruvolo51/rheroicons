#' SOLID SVG Icons
#' @name solid
#' @keywords rheroicons solid
#' @details solid icons from heroicons
#' @usage rheroicons::solid$icon_name()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers
#' @references
#' \url{https://github.com/refactoringui/heroicons}
#' \url{https://heroicons.dev}
#' @examples
#' reheroicons::solid$book_open()
#' reheroicons::solid$book_open(id = 'myBookIcon')
#' reheroicons::solid$book_open(class = 'my-icon-set')
#' @importFrom htmltools tag
#' @export
solid <- list()

#'adjustments
#' @name adjustments
#' @return create a solid or outline SVG icon of a adjustments
#' @usage solid$adjustments()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid adjustments
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/adjustments.svg}
#' \url{https://heroicons.dev}
#' @export
solid$adjustments <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-adjustments", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M5 4a1 1 0 00-2 0v7.268a2 2 0 000 3.464V16a1 1 0 102 0v-1.268a2 2 0 000-3.464V4zM11 4a1 1 0 10-2 0v1.268a2 2 0 000 3.464V16a1 1 0 102 0V8.732a2 2 0 000-3.464V4zM16 3a1 1 0 011 1v7.268a2 2 0 010 3.464V16a1 1 0 11-2 0v-1.268a2 2 0 010-3.464V4a1 1 0 011-1z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'annotation
#' @name annotation
#' @return create a solid or outline SVG icon of a annotation
#' @usage solid$annotation()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid annotation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/annotation.svg}
#' \url{https://heroicons.dev}
#' @export
solid$annotation <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-annotation", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 13V5a2 2 0 00-2-2H4a2 2 0 00-2 2v8a2 2 0 002 2h3l3 3 3-3h3a2 2 0 002-2zM5 7a1 1 0 011-1h8a1 1 0 110 2H6a1 1 0 01-1-1zm1 3a1 1 0 100 2h3a1 1 0 100-2H6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'archive
#' @name archive
#' @return create a solid or outline SVG icon of a archive
#' @usage solid$archive()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid archive
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/archive.svg}
#' \url{https://heroicons.dev}
#' @export
solid$archive <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-archive", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M4 3a2 2 0 100 4h12a2 2 0 100-4H4z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 8h14v7a2 2 0 01-2 2H5a2 2 0 01-2-2V8zm5 3a1 1 0 011-1h2a1 1 0 110 2H9a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_circle_down
#' @name arrow_circle_down
#' @return create a solid or outline SVG icon of a arrow_circle_down
#' @usage solid$arrow_circle_down()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_circle_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_down.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_circle_down <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_circle_down", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-11a1 1 0 10-2 0v3.586L7.707 9.293a1 1 0 00-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L11 10.586V7z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_circle_left
#' @name arrow_circle_left
#' @return create a solid or outline SVG icon of a arrow_circle_left
#' @usage solid$arrow_circle_left()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_circle_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_left.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_circle_left <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_circle_left", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm.707-10.293a1 1 0 00-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L9.414 11H13a1 1 0 100-2H9.414l1.293-1.293z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_circle_right
#' @name arrow_circle_right
#' @return create a solid or outline SVG icon of a arrow_circle_right
#' @usage solid$arrow_circle_right()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_circle_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_right.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_circle_right <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_circle_right", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-8.707l-3-3a1 1 0 00-1.414 1.414L10.586 9H7a1 1 0 100 2h3.586l-1.293 1.293a1 1 0 101.414 1.414l3-3a1 1 0 000-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_circle_up
#' @name arrow_circle_up
#' @return create a solid or outline SVG icon of a arrow_circle_up
#' @usage solid$arrow_circle_up()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_circle_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_up.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_circle_up <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_circle_up", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-8.707l-3-3a1 1 0 00-1.414 0l-3 3a1 1 0 001.414 1.414L9 9.414V13a1 1 0 102 0V9.414l1.293 1.293a1 1 0 001.414-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_down
#' @name arrow_down
#' @return create a solid or outline SVG icon of a arrow_down
#' @usage solid$arrow_down()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_down.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_down <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_down", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M16.707 10.293a1 1 0 010 1.414l-6 6a1 1 0 01-1.414 0l-6-6a1 1 0 111.414-1.414L9 14.586V3a1 1 0 012 0v11.586l4.293-4.293a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_left
#' @name arrow_left
#' @return create a solid or outline SVG icon of a arrow_left
#' @usage solid$arrow_left()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_left.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_left <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_left", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M9.707 16.707a1 1 0 01-1.414 0l-6-6a1 1 0 010-1.414l6-6a1 1 0 011.414 1.414L5.414 9H17a1 1 0 110 2H5.414l4.293 4.293a1 1 0 010 1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_narrow_down
#' @name arrow_narrow_down
#' @return create a solid or outline SVG icon of a arrow_narrow_down
#' @usage solid$arrow_narrow_down()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_narrow_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_down.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_narrow_down <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_narrow_down", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M14.707 12.293a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 111.414-1.414L9 14.586V3a1 1 0 012 0v11.586l2.293-2.293a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_narrow_left
#' @name arrow_narrow_left
#' @return create a solid or outline SVG icon of a arrow_narrow_left
#' @usage solid$arrow_narrow_left()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_narrow_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_left.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_narrow_left <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_narrow_left", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M7.707 14.707a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 1.414L5.414 9H17a1 1 0 110 2H5.414l2.293 2.293a1 1 0 010 1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_narrow_right
#' @name arrow_narrow_right
#' @return create a solid or outline SVG icon of a arrow_narrow_right
#' @usage solid$arrow_narrow_right()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_narrow_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_right.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_narrow_right <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_narrow_right", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M12.293 5.293a1 1 0 011.414 0l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414-1.414L14.586 11H3a1 1 0 110-2h11.586l-2.293-2.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_narrow_up
#' @name arrow_narrow_up
#' @return create a solid or outline SVG icon of a arrow_narrow_up
#' @usage solid$arrow_narrow_up()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_narrow_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_up.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_narrow_up <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_narrow_up", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5.293 7.707a1 1 0 010-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 01-1.414 1.414L11 5.414V17a1 1 0 11-2 0V5.414L6.707 7.707a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_right
#' @name arrow_right
#' @return create a solid or outline SVG icon of a arrow_right
#' @usage solid$arrow_right()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_right.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_right <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_right", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10.293 3.293a1 1 0 011.414 0l6 6a1 1 0 010 1.414l-6 6a1 1 0 01-1.414-1.414L14.586 11H3a1 1 0 110-2h11.586l-4.293-4.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrow_up
#' @name arrow_up
#' @return create a solid or outline SVG icon of a arrow_up
#' @usage solid$arrow_up()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrow_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_up.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrow_up <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrow_up", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3.293 9.707a1 1 0 010-1.414l6-6a1 1 0 011.414 0l6 6a1 1 0 01-1.414 1.414L11 5.414V17a1 1 0 11-2 0V5.414L4.707 9.707a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'arrows_expand
#' @name arrows_expand
#' @return create a solid or outline SVG icon of a arrows_expand
#' @usage solid$arrows_expand()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid arrows_expand
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrows_expand.svg}
#' \url{https://heroicons.dev}
#' @export
solid$arrows_expand <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-arrows_expand", 
        aria_hidden = aria_hidden, width = "19", height = "20", viewBox = "0 0 19 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(stroke = "#374151", 
            `stroke-linecap` = "round", `stroke-linejoin` = "round", `stroke-width` = "2", 
            d = "M3 8V4m0 0h4M3 4l4 4m8 0V4m0 0h-4m4 0l-4 4m-8 4v4m0 0h4m-4 0l4-4m8 4l-4-4m4 4v-4m0 4h-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'at_symbol
#' @name at_symbol
#' @return create a solid or outline SVG icon of a at_symbol
#' @usage solid$at_symbol()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid at_symbol
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/at_symbol.svg}
#' \url{https://heroicons.dev}
#' @export
solid$at_symbol <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-at_symbol", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M14.243 5.757a6 6 0 10-.986 9.284 1 1 0 111.087 1.678A8 8 0 1118 10a3 3 0 01-4.8 2.401A4 4 0 1114 10a1 1 0 102 0c0-1.537-.586-3.07-1.757-4.243zM12 10a2 2 0 10-4 0 2 2 0 004 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'badge_check
#' @name badge_check
#' @return create a solid or outline SVG icon of a badge_check
#' @usage solid$badge_check()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid badge_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/badge_check.svg}
#' \url{https://heroicons.dev}
#' @export
solid$badge_check <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-badge_check", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6.267 3.455a3.066 3.066 0 001.745-.723 3.066 3.066 0 013.976 0 3.066 3.066 0 001.745.723 3.066 3.066 0 012.812 2.812c.051.643.304 1.254.723 1.745a3.066 3.066 0 010 3.976 3.066 3.066 0 00-.723 1.745 3.066 3.066 0 01-2.812 2.812 3.066 3.066 0 00-1.745.723 3.066 3.066 0 01-3.976 0 3.066 3.066 0 00-1.745-.723 3.066 3.066 0 01-2.812-2.812 3.066 3.066 0 00-.723-1.745 3.066 3.066 0 010-3.976 3.066 3.066 0 00.723-1.745 3.066 3.066 0 012.812-2.812zm7.44 5.252a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'ban
#' @name ban
#' @return create a solid or outline SVG icon of a ban
#' @usage solid$ban()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid ban
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/ban.svg}
#' \url{https://heroicons.dev}
#' @export
solid$ban <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-ban", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M13.477 14.89A6 6 0 015.11 6.524l8.367 8.368zm1.414-1.414L6.524 5.11a6 6 0 018.367 8.367zM18 10a8 8 0 11-16 0 8 8 0 0116 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'bell
#' @name bell
#' @return create a solid or outline SVG icon of a bell
#' @usage solid$bell()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid bell
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/bell.svg}
#' \url{https://heroicons.dev}
#' @export
solid$bell <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-bell", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M10 2a6 6 0 00-6 6v3.586l-.707.707A1 1 0 004 14h12a1 1 0 00.707-1.707L16 11.586V8a6 6 0 00-6-6zM10 18a3 3 0 01-3-3h6a3 3 0 01-3 3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'book_open
#' @name book_open
#' @return create a solid or outline SVG icon of a book_open
#' @usage solid$book_open()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid book_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/book_open.svg}
#' \url{https://heroicons.dev}
#' @export
solid$book_open <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-book_open", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M9 4.804A7.968 7.968 0 005.5 4c-1.255 0-2.443.29-3.5.804v10A7.969 7.969 0 015.5 14c1.669 0 3.218.51 4.5 1.385A7.962 7.962 0 0114.5 14c1.255 0 2.443.29 3.5.804v-10A7.968 7.968 0 0014.5 4c-1.255 0-2.443.29-3.5.804V12a1 1 0 11-2 0V4.804z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'bookmark_alt
#' @name bookmark_alt
#' @return create a solid or outline SVG icon of a bookmark_alt
#' @usage solid$bookmark_alt()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid bookmark_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/bookmark_alt.svg}
#' \url{https://heroicons.dev}
#' @export
solid$bookmark_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-bookmark_alt", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 5a2 2 0 012-2h10a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5zm11 1H6v8l4-2 4 2V6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'bookmark
#' @name bookmark
#' @return create a solid or outline SVG icon of a bookmark
#' @usage solid$bookmark()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid bookmark
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/bookmark.svg}
#' \url{https://heroicons.dev}
#' @export
solid$bookmark <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-bookmark", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M5 4a2 2 0 012-2h6a2 2 0 012 2v14l-5-2.5L5 18V4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'briefcase
#' @name briefcase
#' @return create a solid or outline SVG icon of a briefcase
#' @usage solid$briefcase()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid briefcase
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/briefcase.svg}
#' \url{https://heroicons.dev}
#' @export
solid$briefcase <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-briefcase", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6 6V5a3 3 0 013-3h2a3 3 0 013 3v1h2a2 2 0 012 2v3.57A22.952 22.952 0 0110 13a22.95 22.95 0 01-8-1.43V8a2 2 0 012-2h2zm2-1a1 1 0 011-1h2a1 1 0 011 1v1H8V5zm1 5a1 1 0 011-1h.01a1 1 0 110 2H10a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M2 13.692V16a2 2 0 002 2h12a2 2 0 002-2v-2.308A24.974 24.974 0 0110 15c-2.796 0-5.487-.46-8-1.308z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'calendar
#' @name calendar
#' @return create a solid or outline SVG icon of a calendar
#' @usage solid$calendar()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid calendar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/calendar.svg}
#' \url{https://heroicons.dev}
#' @export
solid$calendar <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-calendar", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6 2a1 1 0 00-1 1v1H4a2 2 0 00-2 2v10a2 2 0 002 2h12a2 2 0 002-2V6a2 2 0 00-2-2h-1V3a1 1 0 10-2 0v1H7V3a1 1 0 00-1-1zm0 5a1 1 0 000 2h8a1 1 0 100-2H6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'camera
#' @name camera
#' @return create a solid or outline SVG icon of a camera
#' @usage solid$camera()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid camera
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/camera.svg}
#' \url{https://heroicons.dev}
#' @export
solid$camera <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-camera", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4 5a2 2 0 00-2 2v8a2 2 0 002 2h12a2 2 0 002-2V7a2 2 0 00-2-2h-1.586a1 1 0 01-.707-.293l-1.121-1.121A2 2 0 0011.172 3H8.828a2 2 0 00-1.414.586L6.293 4.707A1 1 0 015.586 5H4zm6 9a3 3 0 100-6 3 3 0 000 6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'cash
#' @name cash
#' @return create a solid or outline SVG icon of a cash
#' @usage solid$cash()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid cash
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cash.svg}
#' \url{https://heroicons.dev}
#' @export
solid$cash <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-cash", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4 4a2 2 0 00-2 2v4a2 2 0 002 2V6h10a2 2 0 00-2-2H4zm2 6a2 2 0 012-2h8a2 2 0 012 2v4a2 2 0 01-2 2H8a2 2 0 01-2-2v-4zm6 4a2 2 0 100-4 2 2 0 000 4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chart_bar
#' @name chart_bar
#' @return create a solid or outline SVG icon of a chart_bar
#' @usage solid$chart_bar()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chart_bar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chart_bar.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chart_bar <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chart_bar", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chart_pie
#' @name chart_pie
#' @return create a solid or outline SVG icon of a chart_pie
#' @usage solid$chart_pie()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chart_pie
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chart_pie.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chart_pie <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chart_pie", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 10a8 8 0 018-8v8h8a8 8 0 11-16 0z")), 
        tag(`_tag_name` = "path", list(d = "M12 2.252A8.014 8.014 0 0117.748 8H12V2.252z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chart_square_bar
#' @name chart_square_bar
#' @return create a solid or outline SVG icon of a chart_square_bar
#' @usage solid$chart_square_bar()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chart_square_bar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chart_square_bar.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chart_square_bar <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chart_square_bar", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 3a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2V5a2 2 0 00-2-2H5zm9 4a1 1 0 10-2 0v6a1 1 0 102 0V7zm-3 2a1 1 0 10-2 0v4a1 1 0 102 0V9zm-3 3a1 1 0 10-2 0v1a1 1 0 102 0v-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chat_alt_2
#' @name chat_alt_2
#' @return create a solid or outline SVG icon of a chat_alt_2
#' @usage solid$chat_alt_2()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chat_alt_2
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chat_alt_2.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chat_alt_2 <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chat_alt_2", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 5a2 2 0 012-2h7a2 2 0 012 2v4a2 2 0 01-2 2H9l-3 3v-3H4a2 2 0 01-2-2V5z")), 
        tag(`_tag_name` = "path", list(d = "M15 7v2a4 4 0 01-4 4H9.828l-1.766 1.767c.28.149.599.233.938.233h2l3 3v-3h2a2 2 0 002-2V9a2 2 0 00-2-2h-1z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chat_alt
#' @name chat_alt
#' @return create a solid or outline SVG icon of a chat_alt
#' @usage solid$chat_alt()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chat_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chat_alt.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chat_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chat_alt", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 5v8a2 2 0 01-2 2h-5l-5 4v-4H4a2 2 0 01-2-2V5a2 2 0 012-2h12a2 2 0 012 2zM7 8H5v2h2V8zm2 0h2v2H9V8zm6 0h-2v2h2V8z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chat
#' @name chat
#' @return create a solid or outline SVG icon of a chat
#' @usage solid$chat()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chat
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chat.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chat <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chat", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 10c0 3.866-3.582 7-8 7a8.841 8.841 0 01-4.083-.98L2 17l1.338-3.123C2.493 12.767 2 11.434 2 10c0-3.866 3.582-7 8-7s8 3.134 8 7zM7 9H5v2h2V9zm8 0h-2v2h2V9zM9 9h2v2H9V9z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'check_circle
#' @name check_circle
#' @return create a solid or outline SVG icon of a check_circle
#' @usage solid$check_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid check_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/check_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$check_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-check_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'check
#' @name check
#' @return create a solid or outline SVG icon of a check
#' @usage solid$check()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/check.svg}
#' \url{https://heroicons.dev}
#' @export
solid$check <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-check", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chevron_down
#' @name chevron_down
#' @return create a solid or outline SVG icon of a chevron_down
#' @usage solid$chevron_down()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chevron_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_down.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chevron_down <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chevron_down", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chevron_left
#' @name chevron_left
#' @return create a solid or outline SVG icon of a chevron_left
#' @usage solid$chevron_left()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chevron_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_left.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chevron_left <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chevron_left", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chevron_right
#' @name chevron_right
#' @return create a solid or outline SVG icon of a chevron_right
#' @usage solid$chevron_right()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chevron_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_right.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chevron_right <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chevron_right", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'chevron_up
#' @name chevron_up
#' @return create a solid or outline SVG icon of a chevron_up
#' @usage solid$chevron_up()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid chevron_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_up.svg}
#' \url{https://heroicons.dev}
#' @export
solid$chevron_up <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-chevron_up", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M14.707 12.707a1 1 0 01-1.414 0L10 9.414l-3.293 3.293a1 1 0 01-1.414-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 010 1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'clipboard_check
#' @name clipboard_check
#' @return create a solid or outline SVG icon of a clipboard_check
#' @usage solid$clipboard_check()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid clipboard_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard_check.svg}
#' \url{https://heroicons.dev}
#' @export
solid$clipboard_check <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-clipboard_check", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M9 2a1 1 0 000 2h2a1 1 0 100-2H9z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v11a2 2 0 01-2 2H6a2 2 0 01-2-2V5zm9.707 5.707a1 1 0 00-1.414-1.414L9 12.586l-1.293-1.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'clipboard_copy
#' @name clipboard_copy
#' @return create a solid or outline SVG icon of a clipboard_copy
#' @usage solid$clipboard_copy()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid clipboard_copy
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard_copy.svg}
#' \url{https://heroicons.dev}
#' @export
solid$clipboard_copy <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-clipboard_copy", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M8 2a1 1 0 000 2h2a1 1 0 100-2H8z")), 
        tag(`_tag_name` = "path", list(d = "M3 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v6h-4.586l1.293-1.293a1 1 0 00-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L10.414 13H15v3a2 2 0 01-2 2H5a2 2 0 01-2-2V5zM15 11h2a1 1 0 110 2h-2v-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'clipboard_list
#' @name clipboard_list
#' @return create a solid or outline SVG icon of a clipboard_list
#' @usage solid$clipboard_list()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid clipboard_list
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard_list.svg}
#' \url{https://heroicons.dev}
#' @export
solid$clipboard_list <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-clipboard_list", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M9 2a1 1 0 000 2h2a1 1 0 100-2H9z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v11a2 2 0 01-2 2H6a2 2 0 01-2-2V5zm3 4a1 1 0 000 2h.01a1 1 0 100-2H7zm3 0a1 1 0 000 2h3a1 1 0 100-2h-3zm-3 4a1 1 0 100 2h.01a1 1 0 100-2H7zm3 0a1 1 0 100 2h3a1 1 0 100-2h-3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'clipboard
#' @name clipboard
#' @return create a solid or outline SVG icon of a clipboard
#' @usage solid$clipboard()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid clipboard
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard.svg}
#' \url{https://heroicons.dev}
#' @export
solid$clipboard <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-clipboard", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M8 3a1 1 0 011-1h2a1 1 0 110 2H9a1 1 0 01-1-1z")), 
        tag(`_tag_name` = "path", list(d = "M6 3a2 2 0 00-2 2v11a2 2 0 002 2h8a2 2 0 002-2V5a2 2 0 00-2-2 3 3 0 01-3 3H9a3 3 0 01-3-3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'clock
#' @name clock
#' @return create a solid or outline SVG icon of a clock
#' @usage solid$clock()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid clock
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clock.svg}
#' \url{https://heroicons.dev}
#' @export
solid$clock <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-clock", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-12a1 1 0 10-2 0v4a1 1 0 00.293.707l2.828 2.829a1 1 0 101.415-1.415L11 9.586V6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'cloud_download
#' @name cloud_download
#' @return create a solid or outline SVG icon of a cloud_download
#' @usage solid$cloud_download()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid cloud_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cloud_download.svg}
#' \url{https://heroicons.dev}
#' @export
solid$cloud_download <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-cloud_download", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M6 13a4 4 0 010-8 4 4 0 118 0 4 4 0 010 8h-3V8a1 1 0 10-2 0v5H6zM9 13h2v2.586l1.293-1.293a1 1 0 011.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 111.414-1.414L9 15.586V13z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'cloud_upload
#' @name cloud_upload
#' @return create a solid or outline SVG icon of a cloud_upload
#' @usage solid$cloud_upload()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid cloud_upload
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cloud_upload.svg}
#' \url{https://heroicons.dev}
#' @export
solid$cloud_upload <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-cloud_upload", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M2 10a4 4 0 004 4h3v3a1 1 0 102 0v-3h3a4 4 0 000-8 4 4 0 00-8 0 4 4 0 00-4 4zm9 4H9V9.414l-1.293 1.293a1 1 0 01-1.414-1.414l3-3a1 1 0 011.414 0l3 3a1 1 0 01-1.414 1.414L11 9.414V14z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'code
#' @name code
#' @return create a solid or outline SVG icon of a code
#' @usage solid$code()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid code
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/code.svg}
#' \url{https://heroicons.dev}
#' @export
solid$code <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-code", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'cog
#' @name cog
#' @return create a solid or outline SVG icon of a cog
#' @usage solid$cog()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid cog
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cog.svg}
#' \url{https://heroicons.dev}
#' @export
solid$cog <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-cog", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M11.49 3.17c-.38-1.56-2.6-1.56-2.98 0a1.532 1.532 0 01-2.286.948c-1.372-.836-2.942.734-2.106 2.106.54.886.061 2.042-.947 2.287-1.561.379-1.561 2.6 0 2.978a1.532 1.532 0 01.947 2.287c-.836 1.372.734 2.942 2.106 2.106a1.532 1.532 0 012.287.947c.379 1.561 2.6 1.561 2.978 0a1.533 1.533 0 012.287-.947c1.372.836 2.942-.734 2.106-2.106a1.533 1.533 0 01.947-2.287c1.561-.379 1.561-2.6 0-2.978a1.532 1.532 0 01-.947-2.287c.836-1.372-.734-2.942-2.106-2.106a1.532 1.532 0 01-2.287-.947zM10 13a3 3 0 100-6 3 3 0 000 6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'collection
#' @name collection
#' @return create a solid or outline SVG icon of a collection
#' @usage solid$collection()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid collection
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/collection.svg}
#' \url{https://heroicons.dev}
#' @export
solid$collection <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-collection", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M7 3a1 1 0 000 2h6a1 1 0 100-2H7zM4 7a1 1 0 011-1h10a1 1 0 110 2H5a1 1 0 01-1-1zM2 11a2 2 0 012-2h12a2 2 0 012 2v4a2 2 0 01-2 2H4a2 2 0 01-2-2v-4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'color_swatch
#' @name color_swatch
#' @return create a solid or outline SVG icon of a color_swatch
#' @usage solid$color_swatch()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid color_swatch
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/color_swatch.svg}
#' \url{https://heroicons.dev}
#' @export
solid$color_swatch <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-color_swatch", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4 2a2 2 0 00-2 2v11a3 3 0 106 0V4a2 2 0 00-2-2H4zm1 14a1 1 0 100-2 1 1 0 000 2zm5-1.757l4.9-4.9a2 2 0 000-2.828L13.485 5.1a2 2 0 00-2.828 0L10 5.757v8.486zM16 18H9.071l6-6H16a2 2 0 012 2v2a2 2 0 01-2 2z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'credit_card
#' @name credit_card
#' @return create a solid or outline SVG icon of a credit_card
#' @usage solid$credit_card()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid credit_card
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/credit_card.svg}
#' \url{https://heroicons.dev}
#' @export
solid$credit_card <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-credit_card", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M4 4a2 2 0 00-2 2v1h16V6a2 2 0 00-2-2H4z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 9H2v5a2 2 0 002 2h12a2 2 0 002-2V9zM4 13a1 1 0 011-1h1a1 1 0 110 2H5a1 1 0 01-1-1zm5-1a1 1 0 100 2h1a1 1 0 100-2H9z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'currency_dollar
#' @name currency_dollar
#' @return create a solid or outline SVG icon of a currency_dollar
#' @usage solid$currency_dollar()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid currency_dollar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_dollar.svg}
#' \url{https://heroicons.dev}
#' @export
solid$currency_dollar <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-currency_dollar", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M8.433 7.418c.155-.103.346-.196.567-.267v1.698a2.305 2.305 0 01-.567-.267C8.07 8.34 8 8.114 8 8c0-.114.07-.34.433-.582zM11 12.849v-1.698c.22.071.412.164.567.267.364.243.433.468.433.582 0 .114-.07.34-.433.582a2.305 2.305 0 01-.567.267z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-13a1 1 0 10-2 0v.092a4.535 4.535 0 00-1.676.662C6.602 6.234 6 7.009 6 8c0 .99.602 1.765 1.324 2.246.48.32 1.054.545 1.676.662v1.941c-.391-.127-.68-.317-.843-.504a1 1 0 10-1.51 1.31c.562.649 1.413 1.076 2.353 1.253V15a1 1 0 102 0v-.092a4.535 4.535 0 001.676-.662C13.398 13.766 14 12.991 14 12c0-.99-.602-1.765-1.324-2.246A4.535 4.535 0 0011 9.092V7.151c.391.127.68.317.843.504a1 1 0 101.511-1.31c-.563-.649-1.413-1.076-2.354-1.253V5z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'currency_euro
#' @name currency_euro
#' @return create a solid or outline SVG icon of a currency_euro
#' @usage solid$currency_euro()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid currency_euro
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_euro.svg}
#' \url{https://heroicons.dev}
#' @export
solid$currency_euro <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-currency_euro", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM8.736 6.979C9.208 6.193 9.696 6 10 6c.304 0 .792.193 1.264.979a1 1 0 001.715-1.029C12.279 4.784 11.232 4 10 4s-2.279.784-2.979 1.95c-.285.475-.507 1-.67 1.55H6a1 1 0 000 2h.013a9.358 9.358 0 000 1H6a1 1 0 100 2h.351c.163.55.385 1.075.67 1.55C7.721 15.216 8.768 16 10 16s2.279-.784 2.979-1.95a1 1 0 10-1.715-1.029c-.472.786-.96.979-1.264.979-.304 0-.792-.193-1.264-.979a4.265 4.265 0 01-.264-.521H10a1 1 0 100-2H8.017a7.36 7.36 0 010-1H10a1 1 0 100-2H8.472c.08-.185.167-.36.264-.521z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'currency_pound
#' @name currency_pound
#' @return create a solid or outline SVG icon of a currency_pound
#' @usage solid$currency_pound()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid currency_pound
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_pound.svg}
#' \url{https://heroicons.dev}
#' @export
solid$currency_pound <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-currency_pound", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-14a3 3 0 00-3 3v2H7a1 1 0 000 2h1v1a1 1 0 01-1 1 1 1 0 100 2h6a1 1 0 100-2H9.83c.11-.313.17-.65.17-1v-1h1a1 1 0 100-2h-1V7a1 1 0 112 0 1 1 0 102 0 3 3 0 00-3-3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'currency_rupee
#' @name currency_rupee
#' @return create a solid or outline SVG icon of a currency_rupee
#' @usage solid$currency_rupee()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid currency_rupee
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_rupee.svg}
#' \url{https://heroicons.dev}
#' @export
solid$currency_rupee <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-currency_rupee", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 5a1 1 0 100 2h1a2 2 0 011.732 1H7a1 1 0 100 2h2.732A2 2 0 018 11H7a1 1 0 00-.707 1.707l3 3a1 1 0 001.414-1.414l-1.483-1.484A4.008 4.008 0 0011.874 10H13a1 1 0 100-2h-1.126a3.976 3.976 0 00-.41-1H13a1 1 0 100-2H7z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'currency_yen
#' @name currency_yen
#' @return create a solid or outline SVG icon of a currency_yen
#' @usage solid$currency_yen()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid currency_yen
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_yen.svg}
#' \url{https://heroicons.dev}
#' @export
solid$currency_yen <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-currency_yen", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7.858 5.485a1 1 0 00-1.715 1.03L7.633 9H7a1 1 0 100 2h1.834l.166.277V12H7a1 1 0 100 2h2v1a1 1 0 102 0v-1h2a1 1 0 100-2h-2v-.723l.166-.277H13a1 1 0 100-2h-.634l1.492-2.486a1 1 0 10-1.716-1.029L10.034 9h-.068L7.858 5.485z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'cursor_click
#' @name cursor_click
#' @return create a solid or outline SVG icon of a cursor_click
#' @usage solid$cursor_click()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid cursor_click
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cursor_click.svg}
#' \url{https://heroicons.dev}
#' @export
solid$cursor_click <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-cursor_click", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6.672 1.911a1 1 0 10-1.932.518l.259.966a1 1 0 001.932-.518l-.26-.966zM2.429 4.74a1 1 0 10-.517 1.932l.966.259a1 1 0 00.517-1.932l-.966-.26zm8.814-.569a1 1 0 00-1.415-1.414l-.707.707a1 1 0 101.415 1.415l.707-.708zm-7.071 7.072l.707-.707A1 1 0 003.465 9.12l-.708.707a1 1 0 001.415 1.415zm3.2-5.171a1 1 0 00-1.3 1.3l4 10a1 1 0 001.823.075l1.38-2.759 3.018 3.02a1 1 0 001.414-1.415l-3.019-3.02 2.76-1.379a1 1 0 00-.076-1.822l-10-4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'desktop_computer
#' @name desktop_computer
#' @return create a solid or outline SVG icon of a desktop_computer
#' @usage solid$desktop_computer()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid desktop_computer
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/desktop_computer.svg}
#' \url{https://heroicons.dev}
#' @export
solid$desktop_computer <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-desktop_computer", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 5a2 2 0 012-2h10a2 2 0 012 2v8a2 2 0 01-2 2h-2.22l.123.489.804.804A1 1 0 0113 18H7a1 1 0 01-.707-1.707l.804-.804L7.22 15H5a2 2 0 01-2-2V5zm5.771 7H5V5h10v7H8.771z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'document_add
#' @name document_add
#' @return create a solid or outline SVG icon of a document_add
#' @usage solid$document_add()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid document_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_add.svg}
#' \url{https://heroicons.dev}
#' @export
solid$document_add <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-document_add", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm5 6a1 1 0 10-2 0v2H7a1 1 0 100 2h2v2a1 1 0 102 0v-2h2a1 1 0 100-2h-2V8z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'document_download
#' @name document_download
#' @return create a solid or outline SVG icon of a document_download
#' @usage solid$document_download()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid document_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_download.svg}
#' \url{https://heroicons.dev}
#' @export
solid$document_download <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-document_download", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm5 6a1 1 0 10-2 0v3.586l-1.293-1.293a1 1 0 10-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L11 11.586V8z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'document_duplicate
#' @name document_duplicate
#' @return create a solid or outline SVG icon of a document_duplicate
#' @usage solid$document_duplicate()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid document_duplicate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_duplicate.svg}
#' \url{https://heroicons.dev}
#' @export
solid$document_duplicate <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-document_duplicate", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M9 2a2 2 0 00-2 2v8a2 2 0 002 2h6a2 2 0 002-2V6.414A2 2 0 0016.414 5L14 2.586A2 2 0 0012.586 2H9z")), 
        tag(`_tag_name` = "path", list(d = "M3 8a2 2 0 012-2v10h8a2 2 0 01-2 2H5a2 2 0 01-2-2V8z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'document_remove
#' @name document_remove
#' @return create a solid or outline SVG icon of a document_remove
#' @usage solid$document_remove()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid document_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_remove.svg}
#' \url{https://heroicons.dev}
#' @export
solid$document_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-document_remove", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm1 8a1 1 0 100 2h6a1 1 0 100-2H7z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'document_report
#' @name document_report
#' @return create a solid or outline SVG icon of a document_report
#' @usage solid$document_report()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid document_report
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_report.svg}
#' \url{https://heroicons.dev}
#' @export
solid$document_report <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-document_report", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm2 10a1 1 0 10-2 0v3a1 1 0 102 0v-3zm2-3a1 1 0 011 1v5a1 1 0 11-2 0v-5a1 1 0 011-1zm4-1a1 1 0 10-2 0v7a1 1 0 102 0V8z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'document
#' @name document
#' @return create a solid or outline SVG icon of a document
#' @usage solid$document()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid document
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document.svg}
#' \url{https://heroicons.dev}
#' @export
solid$document <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-document", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4 4a2 2 0 012-2h4.586A2 2 0 0112 2.586L15.414 6A2 2 0 0116 7.414V16a2 2 0 01-2 2H6a2 2 0 01-2-2V4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'dots_circle_horizontal
#' @name dots_circle_horizontal
#' @return create a solid or outline SVG icon of a dots_circle_horizontal
#' @usage solid$dots_circle_horizontal()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid dots_circle_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/dots_circle_horizontal.svg}
#' \url{https://heroicons.dev}
#' @export
solid$dots_circle_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-dots_circle_horizontal", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9H5v2h2V9zm8 0h-2v2h2V9zM9 9h2v2H9V9z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'dots_horizontal
#' @name dots_horizontal
#' @return create a solid or outline SVG icon of a dots_horizontal
#' @usage solid$dots_horizontal()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid dots_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/dots_horizontal.svg}
#' \url{https://heroicons.dev}
#' @export
solid$dots_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-dots_horizontal", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M6 10a2 2 0 11-4 0 2 2 0 014 0zM12 10a2 2 0 11-4 0 2 2 0 014 0zM16 12a2 2 0 100-4 2 2 0 000 4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'dots_vertical
#' @name dots_vertical
#' @return create a solid or outline SVG icon of a dots_vertical
#' @usage solid$dots_vertical()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid dots_vertical
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/dots_vertical.svg}
#' \url{https://heroicons.dev}
#' @export
solid$dots_vertical <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-dots_vertical", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M10 6a2 2 0 110-4 2 2 0 010 4zM10 12a2 2 0 110-4 2 2 0 010 4zM10 18a2 2 0 110-4 2 2 0 010 4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'download
#' @name download
#' @return create a solid or outline SVG icon of a download
#' @usage solid$download()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/download.svg}
#' \url{https://heroicons.dev}
#' @export
solid$download <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-download", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 17a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm3.293-7.707a1 1 0 011.414 0L9 10.586V3a1 1 0 112 0v7.586l1.293-1.293a1 1 0 111.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'duplicate
#' @name duplicate
#' @return create a solid or outline SVG icon of a duplicate
#' @usage solid$duplicate()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid duplicate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/duplicate.svg}
#' \url{https://heroicons.dev}
#' @export
solid$duplicate <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-duplicate", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M7 9a2 2 0 012-2h6a2 2 0 012 2v6a2 2 0 01-2 2H9a2 2 0 01-2-2V9z")), 
        tag(`_tag_name` = "path", list(d = "M5 3a2 2 0 00-2 2v6a2 2 0 002 2V5h8a2 2 0 00-2-2H5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'emoji_happy
#' @name emoji_happy
#' @return create a solid or outline SVG icon of a emoji_happy
#' @usage solid$emoji_happy()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid emoji_happy
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/emoji_happy.svg}
#' \url{https://heroicons.dev}
#' @export
solid$emoji_happy <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-emoji_happy", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9a1 1 0 100-2 1 1 0 000 2zm7-1a1 1 0 11-2 0 1 1 0 012 0zm-.464 5.535a1 1 0 10-1.415-1.414 3 3 0 01-4.242 0 1 1 0 00-1.415 1.414 5 5 0 007.072 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'emoji_sad
#' @name emoji_sad
#' @return create a solid or outline SVG icon of a emoji_sad
#' @usage solid$emoji_sad()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid emoji_sad
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/emoji_sad.svg}
#' \url{https://heroicons.dev}
#' @export
solid$emoji_sad <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-emoji_sad", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9a1 1 0 100-2 1 1 0 000 2zm7-1a1 1 0 11-2 0 1 1 0 012 0zm-7.536 5.879a1 1 0 001.415 0 3 3 0 014.242 0 1 1 0 001.415-1.415 5 5 0 00-7.072 0 1 1 0 000 1.415z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'exclamation_circle
#' @name exclamation_circle
#' @return create a solid or outline SVG icon of a exclamation_circle
#' @usage solid$exclamation_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid exclamation_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/exclamation_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$exclamation_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-exclamation_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'exclamation
#' @name exclamation
#' @return create a solid or outline SVG icon of a exclamation
#' @usage solid$exclamation()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid exclamation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/exclamation.svg}
#' \url{https://heroicons.dev}
#' @export
solid$exclamation <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-exclamation", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'external_link
#' @name external_link
#' @return create a solid or outline SVG icon of a external_link
#' @usage solid$external_link()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid external_link
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/external_link.svg}
#' \url{https://heroicons.dev}
#' @export
solid$external_link <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-external_link", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z")), 
        tag(`_tag_name` = "path", list(d = "M5 5a2 2 0 00-2 2v8a2 2 0 002 2h8a2 2 0 002-2v-3a1 1 0 10-2 0v3H5V7h3a1 1 0 000-2H5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'eye_off
#' @name eye_off
#' @return create a solid or outline SVG icon of a eye_off
#' @usage solid$eye_off()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid eye_off
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/eye_off.svg}
#' \url{https://heroicons.dev}
#' @export
solid$eye_off <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-eye_off", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3.707 2.293a1 1 0 00-1.414 1.414l14 14a1 1 0 001.414-1.414l-1.473-1.473A10.014 10.014 0 0019.542 10C18.268 5.943 14.478 3 10 3a9.958 9.958 0 00-4.512 1.074l-1.78-1.781zm4.261 4.26l1.514 1.515a2.003 2.003 0 012.45 2.45l1.514 1.514a4 4 0 00-5.478-5.478z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M12.454 16.697L9.75 13.992a4 4 0 01-3.742-3.741L2.335 6.578A9.98 9.98 0 00.458 10c1.274 4.057 5.065 7 9.542 7 .847 0 1.669-.105 2.454-.303z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'eye
#' @name eye
#' @return create a solid or outline SVG icon of a eye
#' @usage solid$eye()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid eye
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/eye.svg}
#' \url{https://heroicons.dev}
#' @export
solid$eye <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-eye", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M10 12a2 2 0 100-4 2 2 0 000 4z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M.458 10C1.732 5.943 5.522 3 10 3s8.268 2.943 9.542 7c-1.274 4.057-5.064 7-9.542 7S1.732 14.057.458 10zM14 10a4 4 0 11-8 0 4 4 0 018 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'filter
#' @name filter
#' @return create a solid or outline SVG icon of a filter
#' @usage solid$filter()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid filter
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/filter.svg}
#' \url{https://heroicons.dev}
#' @export
solid$filter <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-filter", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 3a1 1 0 011-1h12a1 1 0 011 1v3a1 1 0 01-.293.707L12 11.414V15a1 1 0 01-.293.707l-2 2A1 1 0 018 17v-5.586L3.293 6.707A1 1 0 013 6V3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'fire
#' @name fire
#' @return create a solid or outline SVG icon of a fire
#' @usage solid$fire()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid fire
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/fire.svg}
#' \url{https://heroicons.dev}
#' @export
solid$fire <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-fire", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "g", list(`clip-path` = "url(#clip0)", 
            tag(`_tag_name` = "path", list(stroke = "#374151", `stroke-linecap` = "round", 
                `stroke-linejoin` = "round", `stroke-width` = "2", d = "M14.243 15.243a6 6 0 01-8.486-8.486C5.757 9 6 11 9 12c0-2 1-8 2.5-9 1 2 1.571 2.586 2.742 3.757A5.981 5.981 0 0116 11a5.982 5.982 0 01-1.757 4.243z")), 
            tag(`_tag_name` = "path", list(d = "M7.879 15.121a3 3 0 104.242-4.242C11.536 10.293 11.25 10 10.75 9c-.75.5-1.25 3.5-1.25 4.5C7.879 13.5 7 13 7 13c0 .768.293 1.536.879 2.121z")))), 
        tag(`_tag_name` = "defs", list(tag(`_tag_name` = "clippath", list(id = "clip0", 
            tag(`_tag_name` = "path", list(d = "M0 0h20v20H0z"))))))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'flag
#' @name flag
#' @return create a solid or outline SVG icon of a flag
#' @usage solid$flag()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid flag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/flag.svg}
#' \url{https://heroicons.dev}
#' @export
solid$flag <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-flag", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 6a3 3 0 013-3h10a1 1 0 01.8 1.6L14.25 8l2.55 3.4A1 1 0 0116 13H6a1 1 0 00-1 1v3a1 1 0 11-2 0V6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'folder_add
#' @name folder_add
#' @return create a solid or outline SVG icon of a folder_add
#' @usage solid$folder_add()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid folder_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder_add.svg}
#' \url{https://heroicons.dev}
#' @export
solid$folder_add <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-folder_add", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z")), 
        tag(`_tag_name` = "path", list(stroke = "#fff", `stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M8 11h4m-2-2v4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'folder_download
#' @name folder_download
#' @return create a solid or outline SVG icon of a folder_download
#' @usage solid$folder_download()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid folder_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder_download.svg}
#' \url{https://heroicons.dev}
#' @export
solid$folder_download <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-folder_download", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z")), 
        tag(`_tag_name` = "path", list(stroke = "#fff", `stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M10 9v4m0 0l-2-2m2 2l2-2"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'folder_remove
#' @name folder_remove
#' @return create a solid or outline SVG icon of a folder_remove
#' @usage solid$folder_remove()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid folder_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder_remove.svg}
#' \url{https://heroicons.dev}
#' @export
solid$folder_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-folder_remove", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z")), 
        tag(`_tag_name` = "path", list(stroke = "#fff", `stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M8 11h4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'folder
#' @name folder
#' @return create a solid or outline SVG icon of a folder
#' @usage solid$folder()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid folder
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder.svg}
#' \url{https://heroicons.dev}
#' @export
solid$folder <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-folder", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'globe_alt
#' @name globe_alt
#' @return create a solid or outline SVG icon of a globe_alt
#' @usage solid$globe_alt()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid globe_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/globe_alt.svg}
#' \url{https://heroicons.dev}
#' @export
solid$globe_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-globe_alt", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4.083 9h1.946c.089-1.546.383-2.97.837-4.118A6.004 6.004 0 004.083 9zM10 2a8 8 0 100 16 8 8 0 000-16zm0 2c-.076 0-.232.032-.465.262-.238.234-.497.623-.737 1.182-.389.907-.673 2.142-.766 3.556h3.936c-.093-1.414-.377-2.649-.766-3.556-.24-.56-.5-.948-.737-1.182C10.232 4.032 10.076 4 10 4zm3.971 5c-.089-1.546-.383-2.97-.837-4.118A6.004 6.004 0 0115.917 9h-1.946zm-2.003 2H8.032c.093 1.414.377 2.649.766 3.556.24.56.5.948.737 1.182.233.23.389.262.465.262.076 0 .232-.032.465-.262.238-.234.498-.623.737-1.182.389-.907.673-2.142.766-3.556zm1.166 4.118c.454-1.147.748-2.572.837-4.118h1.946a6.004 6.004 0 01-2.783 4.118zm-6.268 0C6.412 13.97 6.118 12.546 6.03 11H4.083a6.004 6.004 0 002.783 4.118z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'globe
#' @name globe
#' @return create a solid or outline SVG icon of a globe
#' @usage solid$globe()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid globe
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/globe.svg}
#' \url{https://heroicons.dev}
#' @export
solid$globe <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-globe", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM4.332 8.027a6.012 6.012 0 011.912-2.706C6.512 5.73 6.974 6 7.5 6A1.5 1.5 0 019 7.5V8a2 2 0 004 0 2 2 0 011.523-1.943A5.977 5.977 0 0116 10c0 .34-.028.675-.083 1H15a2 2 0 00-2 2v2.197A5.973 5.973 0 0110 16v-2a2 2 0 00-2-2 2 2 0 01-2-2 2 2 0 00-1.668-1.973z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'hand
#' @name hand
#' @return create a solid or outline SVG icon of a hand
#' @usage solid$hand()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid hand
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/hand.svg}
#' \url{https://heroicons.dev}
#' @export
solid$hand <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-hand", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M9 3a1 1 0 012 0v5.5a.5.5 0 001 0V4a1 1 0 112 0v4.5a.5.5 0 001 0V6a1 1 0 112 0v5a7 7 0 11-14 0V9a1 1 0 012 0v2.5a.5.5 0 001 0V4a1 1 0 012 0v4.5a.5.5 0 001 0V3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'hashtag
#' @name hashtag
#' @return create a solid or outline SVG icon of a hashtag
#' @usage solid$hashtag()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid hashtag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/hashtag.svg}
#' \url{https://heroicons.dev}
#' @export
solid$hashtag <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-hashtag", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M9.243 3.03a1 1 0 01.727 1.213L9.53 6h2.94l.56-2.243a1 1 0 111.94.486L14.53 6H17a1 1 0 110 2h-2.97l-1 4H15a1 1 0 110 2h-2.47l-.56 2.242a1 1 0 11-1.94-.485L10.47 14H7.53l-.56 2.242a1 1 0 11-1.94-.485L5.47 14H3a1 1 0 110-2h2.97l1-4H5a1 1 0 110-2h2.47l.56-2.243a1 1 0 011.213-.727zM9.03 8l-1 4h2.938l1-4H9.031z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'heart
#' @name heart
#' @return create a solid or outline SVG icon of a heart
#' @usage solid$heart()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid heart
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/heart.svg}
#' \url{https://heroicons.dev}
#' @export
solid$heart <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-heart", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'home
#' @name home
#' @return create a solid or outline SVG icon of a home
#' @usage solid$home()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid home
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/home.svg}
#' \url{https://heroicons.dev}
#' @export
solid$home <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-home", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M10.707 2.293a1 1 0 00-1.414 0l-7 7a1 1 0 001.414 1.414L4 10.414V17a1 1 0 001 1h2a1 1 0 001-1v-2a1 1 0 011-1h2a1 1 0 011 1v2a1 1 0 001 1h2a1 1 0 001-1v-6.586l.293.293a1 1 0 001.414-1.414l-7-7z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'inbox_in
#' @name inbox_in
#' @return create a solid or outline SVG icon of a inbox_in
#' @usage solid$inbox_in()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid inbox_in
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/inbox_in.svg}
#' \url{https://heroicons.dev}
#' @export
solid$inbox_in <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-inbox_in", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M8.707 7.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l2-2a1 1 0 00-1.414-1.414L11 7.586V3a1 1 0 10-2 0v4.586l-.293-.293z")), 
        tag(`_tag_name` = "path", list(d = "M3 5a2 2 0 012-2h1a1 1 0 010 2H5v7h2l1 2h4l1-2h2V5h-1a1 1 0 110-2h1a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'inbox
#' @name inbox
#' @return create a solid or outline SVG icon of a inbox
#' @usage solid$inbox()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid inbox
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/inbox.svg}
#' \url{https://heroicons.dev}
#' @export
solid$inbox <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-inbox", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 3a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2V5a2 2 0 00-2-2H5zm0 2h10v7h-2l-1 2H8l-1-2H5V5z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'information_circle
#' @name information_circle
#' @return create a solid or outline SVG icon of a information_circle
#' @usage solid$information_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid information_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/information_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$information_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-information_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'key
#' @name key
#' @return create a solid or outline SVG icon of a key
#' @usage solid$key()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid key
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/key.svg}
#' \url{https://heroicons.dev}
#' @export
solid$key <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-key", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 8a6 6 0 01-7.743 5.743L10 14l-1 1-1 1H6v2H2v-4l4.257-4.257A6 6 0 1118 8zm-6-4a1 1 0 100 2 2 2 0 012 2 1 1 0 102 0 4 4 0 00-4-4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'library
#' @name library
#' @return create a solid or outline SVG icon of a library
#' @usage solid$library()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid library
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/library.svg}
#' \url{https://heroicons.dev}
#' @export
solid$library <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-library", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10.496 2.132a1 1 0 00-.992 0l-7 4A1 1 0 003 8v7a1 1 0 100 2h14a1 1 0 100-2V8a1 1 0 00.496-1.868l-7-4zM6 9a1 1 0 00-1 1v3a1 1 0 102 0v-3a1 1 0 00-1-1zm3 1a1 1 0 012 0v3a1 1 0 11-2 0v-3zm5-1a1 1 0 00-1 1v3a1 1 0 102 0v-3a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'light_bulb
#' @name light_bulb
#' @return create a solid or outline SVG icon of a light_bulb
#' @usage solid$light_bulb()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid light_bulb
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/light_bulb.svg}
#' \url{https://heroicons.dev}
#' @export
solid$light_bulb <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-light_bulb", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M11 3a1 1 0 10-2 0v1a1 1 0 102 0V3zM15.657 5.757a1 1 0 00-1.414-1.414l-.707.707a1 1 0 001.414 1.414l.707-.707zM18 10a1 1 0 01-1 1h-1a1 1 0 110-2h1a1 1 0 011 1zM5.05 6.464A1 1 0 106.464 5.05l-.707-.707a1 1 0 00-1.414 1.414l.707.707zM5 10a1 1 0 01-1 1H3a1 1 0 110-2h1a1 1 0 011 1zM8 16v-1h4v1a2 2 0 11-4 0zM12 14c.015-.34.208-.646.477-.859a4 4 0 10-4.954 0c.27.213.462.519.476.859h4.002z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'lightning_bolt
#' @name lightning_bolt
#' @return create a solid or outline SVG icon of a lightning_bolt
#' @usage solid$lightning_bolt()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid lightning_bolt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/lightning_bolt.svg}
#' \url{https://heroicons.dev}
#' @export
solid$lightning_bolt <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-lightning_bolt", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M11.3 1.046A1 1 0 0112 2v5h4a1 1 0 01.82 1.573l-7 10A1 1 0 018 18v-5H4a1 1 0 01-.82-1.573l7-10a1 1 0 011.12-.38z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'link
#' @name link
#' @return create a solid or outline SVG icon of a link
#' @usage solid$link()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid link
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/link.svg}
#' \url{https://heroicons.dev}
#' @export
solid$link <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-link", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'location_marker
#' @name location_marker
#' @return create a solid or outline SVG icon of a location_marker
#' @usage solid$location_marker()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid location_marker
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/location_marker.svg}
#' \url{https://heroicons.dev}
#' @export
solid$location_marker <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-location_marker", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5.05 4.05a7 7 0 119.9 9.9L10 18.9l-4.95-4.95a7 7 0 010-9.9zM10 11a2 2 0 100-4 2 2 0 000 4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'lock_closed
#' @name lock_closed
#' @return create a solid or outline SVG icon of a lock_closed
#' @usage solid$lock_closed()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid lock_closed
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/lock_closed.svg}
#' \url{https://heroicons.dev}
#' @export
solid$lock_closed <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-lock_closed", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 9V7a5 5 0 0110 0v2a2 2 0 012 2v5a2 2 0 01-2 2H5a2 2 0 01-2-2v-5a2 2 0 012-2zm8-2v2H7V7a3 3 0 016 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'lock_open
#' @name lock_open
#' @return create a solid or outline SVG icon of a lock_open
#' @usage solid$lock_open()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid lock_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/lock_open.svg}
#' \url{https://heroicons.dev}
#' @export
solid$lock_open <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-lock_open", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M10 2a5 5 0 00-5 5v2a2 2 0 00-2 2v5a2 2 0 002 2h10a2 2 0 002-2v-5a2 2 0 00-2-2H7V7a3 3 0 015.905-.75 1 1 0 001.937-.5A5.002 5.002 0 0010 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'logout
#' @name logout
#' @return create a solid or outline SVG icon of a logout
#' @usage solid$logout()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid logout
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/logout.svg}
#' \url{https://heroicons.dev}
#' @export
solid$logout <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-logout", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 3a1 1 0 011 1v12a1 1 0 11-2 0V4a1 1 0 011-1zm7.707 3.293a1 1 0 010 1.414L9.414 9H17a1 1 0 110 2H9.414l1.293 1.293a1 1 0 01-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'mail_open
#' @name mail_open
#' @return create a solid or outline SVG icon of a mail_open
#' @usage solid$mail_open()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid mail_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/mail_open.svg}
#' \url{https://heroicons.dev}
#' @export
solid$mail_open <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-mail_open", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M2.94 6.412A2 2 0 002 8.108V16a2 2 0 002 2h12a2 2 0 002-2V8.108a2 2 0 00-.94-1.696l-6-3.75a2 2 0 00-2.12 0l-6 3.75zm2.615 2.423a1 1 0 10-1.11 1.664l5 3.333a1 1 0 001.11 0l5-3.333a1 1 0 00-1.11-1.664L10 11.798 5.555 8.835z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'mail
#' @name mail
#' @return create a solid or outline SVG icon of a mail
#' @usage solid$mail()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid mail
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/mail.svg}
#' \url{https://heroicons.dev}
#' @export
solid$mail <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-mail", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2.003 5.884L10 9.882l7.997-3.998A2 2 0 0016 4H4a2 2 0 00-1.997 1.884z")), 
        tag(`_tag_name` = "path", list(d = "M18 8.118l-8 4-8-4V14a2 2 0 002 2h12a2 2 0 002-2V8.118z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'menu_alt_1
#' @name menu_alt_1
#' @return create a solid or outline SVG icon of a menu_alt_1
#' @usage solid$menu_alt_1()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid menu_alt_1
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_1.svg}
#' \url{https://heroicons.dev}
#' @export
solid$menu_alt_1 <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-menu_alt_1", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h6a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'menu_alt_2
#' @name menu_alt_2
#' @return create a solid or outline SVG icon of a menu_alt_2
#' @usage solid$menu_alt_2()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid menu_alt_2
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_2.svg}
#' \url{https://heroicons.dev}
#' @export
solid$menu_alt_2 <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-menu_alt_2", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h6a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'menu_alt_3
#' @name menu_alt_3
#' @return create a solid or outline SVG icon of a menu_alt_3
#' @usage solid$menu_alt_3()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid menu_alt_3
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_3.svg}
#' \url{https://heroicons.dev}
#' @export
solid$menu_alt_3 <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-menu_alt_3", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM9 15a1 1 0 011-1h6a1 1 0 110 2h-6a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'menu_alt_4
#' @name menu_alt_4
#' @return create a solid or outline SVG icon of a menu_alt_4
#' @usage solid$menu_alt_4()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid menu_alt_4
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_4.svg}
#' \url{https://heroicons.dev}
#' @export
solid$menu_alt_4 <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-menu_alt_4", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 7a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 13a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'menu
#' @name menu
#' @return create a solid or outline SVG icon of a menu
#' @usage solid$menu()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid menu
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu.svg}
#' \url{https://heroicons.dev}
#' @export
solid$menu <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-menu", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'microphone
#' @name microphone
#' @return create a solid or outline SVG icon of a microphone
#' @usage solid$microphone()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid microphone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/microphone.svg}
#' \url{https://heroicons.dev}
#' @export
solid$microphone <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-microphone", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M7 4a3 3 0 016 0v4a3 3 0 11-6 0V4zm4 10.93A7.001 7.001 0 0017 8a1 1 0 10-2 0A5 5 0 015 8a1 1 0 00-2 0 7.001 7.001 0 006 6.93V17H6a1 1 0 100 2h8a1 1 0 100-2h-3v-2.07z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'minus_circle
#' @name minus_circle
#' @return create a solid or outline SVG icon of a minus_circle
#' @usage solid$minus_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid minus_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/minus_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$minus_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-minus_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9a1 1 0 000 2h6a1 1 0 100-2H7z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'moon
#' @name moon
#' @return create a solid or outline SVG icon of a moon
#' @usage solid$moon()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid moon
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/moon.svg}
#' \url{https://heroicons.dev}
#' @export
solid$moon <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-moon", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'newspaper
#' @name newspaper
#' @return create a solid or outline SVG icon of a newspaper
#' @usage solid$newspaper()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid newspaper
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/newspaper.svg}
#' \url{https://heroicons.dev}
#' @export
solid$newspaper <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-newspaper", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M2 5a2 2 0 012-2h8a2 2 0 012 2v10a2 2 0 002 2H4a2 2 0 01-2-2V5zm3 1h6v4H5V6zm6 6H5v2h6v-2z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M15 7h1a2 2 0 012 2v5.5a1.5 1.5 0 01-3 0V7z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'office_building
#' @name office_building
#' @return create a solid or outline SVG icon of a office_building
#' @usage solid$office_building()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid office_building
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/office_building.svg}
#' \url{https://heroicons.dev}
#' @export
solid$office_building <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-office_building", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4 4a2 2 0 012-2h8a2 2 0 012 2v12a1 1 0 110 2h-3a1 1 0 01-1-1v-2a1 1 0 00-1-1H9a1 1 0 00-1 1v2a1 1 0 01-1 1H4a1 1 0 110-2V4zm3 1h2v2H7V5zm2 4H7v2h2V9zm2-4h2v2h-2V5zm2 4h-2v2h2V9z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'paper_clip
#' @name paper_clip
#' @return create a solid or outline SVG icon of a paper_clip
#' @usage solid$paper_clip()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid paper_clip
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/paper_clip.svg}
#' \url{https://heroicons.dev}
#' @export
solid$paper_clip <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-paper_clip", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M8 4a3 3 0 00-3 3v4a5 5 0 0010 0V7a1 1 0 112 0v4a7 7 0 11-14 0V7a5 5 0 0110 0v4a3 3 0 11-6 0V7a1 1 0 012 0v4a1 1 0 102 0V7a3 3 0 00-3-3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'pause
#' @name pause
#' @return create a solid or outline SVG icon of a pause
#' @usage solid$pause()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid pause
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/pause.svg}
#' \url{https://heroicons.dev}
#' @export
solid$pause <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-pause", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zM7 8a1 1 0 012 0v4a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v4a1 1 0 102 0V8a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'pencil_alt
#' @name pencil_alt
#' @return create a solid or outline SVG icon of a pencil_alt
#' @usage solid$pencil_alt()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid pencil_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/pencil_alt.svg}
#' \url{https://heroicons.dev}
#' @export
solid$pencil_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-pencil_alt", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M17.414 2.586a2 2 0 00-2.828 0L7 10.172V13h2.828l7.586-7.586a2 2 0 000-2.828z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2 6a2 2 0 012-2h4a1 1 0 010 2H4v10h10v-4a1 1 0 112 0v4a2 2 0 01-2 2H4a2 2 0 01-2-2V6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'pencil
#' @name pencil
#' @return create a solid or outline SVG icon of a pencil
#' @usage solid$pencil()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid pencil
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/pencil.svg}
#' \url{https://heroicons.dev}
#' @export
solid$pencil <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-pencil", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M13.586 3.586a2 2 0 112.828 2.828l-.793.793-2.828-2.828.793-.793zM11.379 5.793L3 14.172V17h2.828l8.38-8.379-2.83-2.828z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'phone_incoming
#' @name phone_incoming
#' @return create a solid or outline SVG icon of a phone_incoming
#' @usage solid$phone_incoming()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid phone_incoming
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/phone_incoming.svg}
#' \url{https://heroicons.dev}
#' @export
solid$phone_incoming <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-phone_incoming", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M14.414 7l3.293-3.293a1 1 0 00-1.414-1.414L13 5.586V4a1 1 0 10-2 0v4.003a.996.996 0 00.617.921A.997.997 0 0012 9h4a1 1 0 100-2h-1.586z")), 
        tag(`_tag_name` = "path", list(d = "M2 3a1 1 0 011-1h2.153a1 1 0 01.986.836l.74 4.435a1 1 0 01-.54 1.06l-1.548.773a11.037 11.037 0 006.105 6.105l.774-1.548a1 1 0 011.059-.54l4.435.74a1 1 0 01.836.986V17a1 1 0 01-1 1h-2C7.82 18 2 12.18 2 5V3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'phone_outgoing
#' @name phone_outgoing
#' @return create a solid or outline SVG icon of a phone_outgoing
#' @usage solid$phone_outgoing()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid phone_outgoing
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/phone_outgoing.svg}
#' \url{https://heroicons.dev}
#' @export
solid$phone_outgoing <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-phone_outgoing", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M17.924 2.617a.997.997 0 00-.215-.322l-.004-.004A.997.997 0 0017 2h-4a1 1 0 100 2h1.586l-3.293 3.293a1 1 0 001.414 1.414L16 5.414V7a1 1 0 102 0V3a.997.997 0 00-.076-.383z")), 
        tag(`_tag_name` = "path", list(d = "M2 3a1 1 0 011-1h2.153a1 1 0 01.986.836l.74 4.435a1 1 0 01-.54 1.06l-1.548.773a11.037 11.037 0 006.105 6.105l.774-1.548a1 1 0 011.059-.54l4.435.74a1 1 0 01.836.986V17a1 1 0 01-1 1h-2C7.82 18 2 12.18 2 5V3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'phone
#' @name phone
#' @return create a solid or outline SVG icon of a phone
#' @usage solid$phone()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid phone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/phone.svg}
#' \url{https://heroicons.dev}
#' @export
solid$phone <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-phone", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 3a1 1 0 011-1h2.153a1 1 0 01.986.836l.74 4.435a1 1 0 01-.54 1.06l-1.548.773a11.037 11.037 0 006.105 6.105l.774-1.548a1 1 0 011.059-.54l4.435.74a1 1 0 01.836.986V17a1 1 0 01-1 1h-2C7.82 18 2 12.18 2 5V3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'photograph
#' @name photograph
#' @return create a solid or outline SVG icon of a photograph
#' @usage solid$photograph()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid photograph
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/photograph.svg}
#' \url{https://heroicons.dev}
#' @export
solid$photograph <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-photograph", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4 3a2 2 0 00-2 2v10a2 2 0 002 2h12a2 2 0 002-2V5a2 2 0 00-2-2H4zm12 12H4l4-8 3 6 2-4 3 6z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'play
#' @name play
#' @return create a solid or outline SVG icon of a play
#' @usage solid$play()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid play
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/play.svg}
#' \url{https://heroicons.dev}
#' @export
solid$play <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-play", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM9.555 7.168A1 1 0 008 8v4a1 1 0 001.555.832l3-2a1 1 0 000-1.664l-3-2z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'plus_circle
#' @name plus_circle
#' @return create a solid or outline SVG icon of a plus_circle
#' @usage solid$plus_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid plus_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/plus_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$plus_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-plus_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-11a1 1 0 10-2 0v2H7a1 1 0 100 2h2v2a1 1 0 102 0v-2h2a1 1 0 100-2h-2V7z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'plus
#' @name plus
#' @return create a solid or outline SVG icon of a plus
#' @usage solid$plus()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid plus
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/plus.svg}
#' \url{https://heroicons.dev}
#' @export
solid$plus <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-plus", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'printer
#' @name printer
#' @return create a solid or outline SVG icon of a printer
#' @usage solid$printer()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid printer
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/printer.svg}
#' \url{https://heroicons.dev}
#' @export
solid$printer <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-printer", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 4v3H4a2 2 0 00-2 2v3a2 2 0 002 2h1v2a2 2 0 002 2h6a2 2 0 002-2v-2h1a2 2 0 002-2V9a2 2 0 00-2-2h-1V4a2 2 0 00-2-2H7a2 2 0 00-2 2zm8 0H7v3h6V4zm0 8H7v4h6v-4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'puzzle
#' @name puzzle
#' @return create a solid or outline SVG icon of a puzzle
#' @usage solid$puzzle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid puzzle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/puzzle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$puzzle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-puzzle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M10 3.5a1.5 1.5 0 013 0V4a1 1 0 001 1h3a1 1 0 011 1v3a1 1 0 01-1 1h-.5a1.5 1.5 0 000 3h.5a1 1 0 011 1v3a1 1 0 01-1 1h-3a1 1 0 01-1-1v-.5a1.5 1.5 0 00-3 0v.5a1 1 0 01-1 1H6a1 1 0 01-1-1v-3a1 1 0 00-1-1h-.5a1.5 1.5 0 010-3H4a1 1 0 001-1V6a1 1 0 011-1h3a1 1 0 001-1v-.5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'qrcode
#' @name qrcode
#' @return create a solid or outline SVG icon of a qrcode
#' @usage solid$qrcode()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid qrcode
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/qrcode.svg}
#' \url{https://heroicons.dev}
#' @export
solid$qrcode <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-qrcode", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 4a1 1 0 011-1h3a1 1 0 011 1v3a1 1 0 01-1 1H4a1 1 0 01-1-1V4zm2 2V5h1v1H5zM3 13a1 1 0 011-1h3a1 1 0 011 1v3a1 1 0 01-1 1H4a1 1 0 01-1-1v-3zm2 2v-1h1v1H5zM13 3a1 1 0 00-1 1v3a1 1 0 001 1h3a1 1 0 001-1V4a1 1 0 00-1-1h-3zm1 2v1h1V5h-1z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M11 4a1 1 0 10-2 0v1a1 1 0 002 0V4zM10 7a1 1 0 011 1v1h2a1 1 0 110 2h-3a1 1 0 01-1-1V8a1 1 0 011-1zM16 9a1 1 0 100 2 1 1 0 000-2zM9 13a1 1 0 011-1h1a1 1 0 110 2v2a1 1 0 11-2 0v-3zM7 11a1 1 0 100-2H4a1 1 0 100 2h3zM17 13a1 1 0 01-1 1h-2a1 1 0 110-2h2a1 1 0 011 1zM16 17a1 1 0 100-2h-3a1 1 0 100 2h3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'question_mark_circle
#' @name question_mark_circle
#' @return create a solid or outline SVG icon of a question_mark_circle
#' @usage solid$question_mark_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid question_mark_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/question_mark_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$question_mark_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-question_mark_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'receipt_refund
#' @name receipt_refund
#' @return create a solid or outline SVG icon of a receipt_refund
#' @usage solid$receipt_refund()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid receipt_refund
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/receipt_refund.svg}
#' \url{https://heroicons.dev}
#' @export
solid$receipt_refund <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-receipt_refund", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 2a2 2 0 00-2 2v14l3.5-2 3.5 2 3.5-2 3.5 2V4a2 2 0 00-2-2H5zm4.707 3.707a1 1 0 00-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L8.414 9H10a3 3 0 013 3v1a1 1 0 102 0v-1a5 5 0 00-5-5H8.414l1.293-1.293z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'refresh
#' @name refresh
#' @return create a solid or outline SVG icon of a refresh
#' @usage solid$refresh()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid refresh
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/refresh.svg}
#' \url{https://heroicons.dev}
#' @export
solid$refresh <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-refresh", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4 2a1 1 0 011 1v2.101a7.002 7.002 0 0111.601 2.566 1 1 0 11-1.885.666A5.002 5.002 0 005.999 7H9a1 1 0 010 2H4a1 1 0 01-1-1V3a1 1 0 011-1zm.008 9.057a1 1 0 011.276.61A5.002 5.002 0 0014.001 13H11a1 1 0 110-2h5a1 1 0 011 1v5a1 1 0 11-2 0v-2.101a7.002 7.002 0 01-11.601-2.566 1 1 0 01.61-1.276z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'reply
#' @name reply
#' @return create a solid or outline SVG icon of a reply
#' @usage solid$reply()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid reply
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/reply.svg}
#' \url{https://heroicons.dev}
#' @export
solid$reply <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-reply", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M7.707 3.293a1 1 0 010 1.414L5.414 7H11a7 7 0 017 7v2a1 1 0 11-2 0v-2a5 5 0 00-5-5H5.414l2.293 2.293a1 1 0 11-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'scale
#' @name scale
#' @return create a solid or outline SVG icon of a scale
#' @usage solid$scale()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid scale
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/scale.svg}
#' \url{https://heroicons.dev}
#' @export
solid$scale <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-scale", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 2a1 1 0 011 1v1.323l3.954 1.582 1.599-.8a1 1 0 01.894 1.79l-1.233.616 1.738 5.42a1 1 0 01-.285 1.05A3.989 3.989 0 0115 15a3.989 3.989 0 01-2.667-1.019 1 1 0 01-.285-1.05l1.715-5.349L11 6.477V16h2a1 1 0 110 2H7a1 1 0 110-2h2V6.477L6.237 7.582l1.715 5.349a1 1 0 01-.285 1.05A3.989 3.989 0 015 15a3.989 3.989 0 01-2.667-1.019 1 1 0 01-.285-1.05l1.738-5.42-1.233-.617a1 1 0 01.894-1.788l1.599.799L9 4.323V3a1 1 0 011-1zm-5 8.274l-.818 2.552c.25.112.526.174.818.174.292 0 .569-.062.818-.174L5 10.274zm10 0l-.818 2.552c.25.112.526.174.818.174.292 0 .569-.062.818-.174L15 10.274z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'search
#' @name search
#' @return create a solid or outline SVG icon of a search
#' @usage solid$search()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid search
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/search.svg}
#' \url{https://heroicons.dev}
#' @export
solid$search <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-search", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'selector
#' @name selector
#' @return create a solid or outline SVG icon of a selector
#' @usage solid$selector()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid selector
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/selector.svg}
#' \url{https://heroicons.dev}
#' @export
solid$selector <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-selector", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 3a1 1 0 01.707.293l3 3a1 1 0 01-1.414 1.414L10 5.414 7.707 7.707a1 1 0 01-1.414-1.414l3-3A1 1 0 0110 3zm-3.707 9.293a1 1 0 011.414 0L10 14.586l2.293-2.293a1 1 0 011.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'share
#' @name share
#' @return create a solid or outline SVG icon of a share
#' @usage solid$share()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid share
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/share.svg}
#' \url{https://heroicons.dev}
#' @export
solid$share <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-share", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M15 8a3 3 0 10-2.977-2.63l-4.94 2.47a3 3 0 100 4.319l4.94 2.47a3 3 0 10.895-1.789l-4.94-2.47a3.027 3.027 0 000-.74l4.94-2.47C13.456 7.68 14.19 8 15 8z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'shield_check
#' @name shield_check
#' @return create a solid or outline SVG icon of a shield_check
#' @usage solid$shield_check()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid shield_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shield_check.svg}
#' \url{https://heroicons.dev}
#' @export
solid$shield_check <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-shield_check", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M2.166 4.999A11.954 11.954 0 0010 1.944 11.954 11.954 0 0017.834 5c.11.65.166 1.32.166 2.001 0 5.225-3.34 9.67-8 11.317C5.34 16.67 2 12.225 2 7c0-.682.057-1.35.166-2.001zm11.541 3.708a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'shield_exclamation
#' @name shield_exclamation
#' @return create a solid or outline SVG icon of a shield_exclamation
#' @usage solid$shield_exclamation()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid shield_exclamation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shield_exclamation.svg}
#' \url{https://heroicons.dev}
#' @export
solid$shield_exclamation <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-shield_exclamation", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 1.944A11.954 11.954 0 012.166 5C2.056 5.649 2 6.319 2 7c0 5.225 3.34 9.67 8 11.317C14.66 16.67 18 12.225 18 7c0-.682-.057-1.35-.166-2.001A11.954 11.954 0 0110 1.944zM11 14a1 1 0 11-2 0 1 1 0 012 0zm0-7a1 1 0 10-2 0v3a1 1 0 102 0V7z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'shopping_bag
#' @name shopping_bag
#' @return create a solid or outline SVG icon of a shopping_bag
#' @usage solid$shopping_bag()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid shopping_bag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shopping_bag.svg}
#' \url{https://heroicons.dev}
#' @export
solid$shopping_bag <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-shopping_bag", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(stroke = "#374151", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M5 8h10l1 9H4l1-9z")), 
        tag(`_tag_name` = "path", list(stroke = "#374151", `stroke-width` = "2", 
            d = "M7 6a3 3 0 013-3v0a3 3 0 013 3v3a3 3 0 01-3 3v0a3 3 0 01-3-3V6z")), 
        tag(`_tag_name` = "rect", list(width = "2", height = "2", x = "6", y = "9", 
            rx = "1")), tag(`_tag_name` = "rect", list(width = "2", height = "2", 
            x = "12", y = "9", rx = "1"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'shopping_cart
#' @name shopping_cart
#' @return create a solid or outline SVG icon of a shopping_cart
#' @usage solid$shopping_cart()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid shopping_cart
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shopping_cart.svg}
#' \url{https://heroicons.dev}
#' @export
solid$shopping_cart <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-shopping_cart", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M3 1a1 1 0 000 2h1.22l.305 1.222a.997.997 0 00.01.042l1.358 5.43-.893.892C3.74 11.846 4.632 14 6.414 14H15a1 1 0 000-2H6.414l1-1H14a1 1 0 00.894-.553l3-6A1 1 0 0017 3H6.28l-.31-1.243A1 1 0 005 1H3zM16 16.5a1.5 1.5 0 11-3 0 1.5 1.5 0 013 0zM6.5 18a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'sort_ascending
#' @name sort_ascending
#' @return create a solid or outline SVG icon of a sort_ascending
#' @usage solid$sort_ascending()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid sort_ascending
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sort_ascending.svg}
#' \url{https://heroicons.dev}
#' @export
solid$sort_ascending <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-sort_ascending", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M3 3a1 1 0 000 2h11a1 1 0 100-2H3zM3 7a1 1 0 000 2h5a1 1 0 000-2H3zM3 11a1 1 0 100 2h4a1 1 0 100-2H3zM13 16a1 1 0 102 0v-5.586l1.293 1.293a1 1 0 001.414-1.414l-3-3a1 1 0 00-1.414 0l-3 3a1 1 0 101.414 1.414L13 10.414V16z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'sort_descending
#' @name sort_descending
#' @return create a solid or outline SVG icon of a sort_descending
#' @usage solid$sort_descending()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid sort_descending
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sort_descending.svg}
#' \url{https://heroicons.dev}
#' @export
solid$sort_descending <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-sort_descending", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M3 3a1 1 0 000 2h11a1 1 0 100-2H3zM3 7a1 1 0 000 2h7a1 1 0 100-2H3zM3 11a1 1 0 100 2h4a1 1 0 100-2H3zM15 8a1 1 0 10-2 0v5.586l-1.293-1.293a1 1 0 00-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L15 13.586V8z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'sparkles
#' @name sparkles
#' @return create a solid or outline SVG icon of a sparkles
#' @usage solid$sparkles()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid sparkles
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sparkles.svg}
#' \url{https://heroicons.dev}
#' @export
solid$sparkles <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-sparkles", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 2a1 1 0 011 1v1h1a1 1 0 010 2H6v1a1 1 0 01-2 0V6H3a1 1 0 010-2h1V3a1 1 0 011-1zm0 10a1 1 0 011 1v1h1a1 1 0 110 2H6v1a1 1 0 11-2 0v-1H3a1 1 0 110-2h1v-1a1 1 0 011-1zM12 2a1 1 0 01.967.744L14.146 7.2 17.5 9.134a1 1 0 010 1.732l-3.354 1.935-1.18 4.455a1 1 0 01-1.933 0L9.854 12.8 6.5 10.866a1 1 0 010-1.732l3.354-1.935 1.18-4.455A1 1 0 0112 2z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'speakerphone
#' @name speakerphone
#' @return create a solid or outline SVG icon of a speakerphone
#' @usage solid$speakerphone()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid speakerphone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/speakerphone.svg}
#' \url{https://heroicons.dev}
#' @export
solid$speakerphone <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-speakerphone", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 3a1 1 0 00-1.447-.894L8.763 6H5a3 3 0 000 6h.28l1.771 5.316A1 1 0 008 18h1a1 1 0 001-1v-4.382l6.553 3.276A1 1 0 0018 15V3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'star
#' @name star
#' @return create a solid or outline SVG icon of a star
#' @usage solid$star()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid star
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/star.svg}
#' \url{https://heroicons.dev}
#' @export
solid$star <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-star", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'stop
#' @name stop
#' @return create a solid or outline SVG icon of a stop
#' @usage solid$stop()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid stop
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/stop.svg}
#' \url{https://heroicons.dev}
#' @export
solid$stop <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-stop", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM8 7a1 1 0 00-1 1v4a1 1 0 001 1h4a1 1 0 001-1V8a1 1 0 00-1-1H8z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'sun
#' @name sun
#' @return create a solid or outline SVG icon of a sun
#' @usage solid$sun()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid sun
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sun.svg}
#' \url{https://heroicons.dev}
#' @export
solid$sun <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-sun", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'support
#' @name support
#' @return create a solid or outline SVG icon of a support
#' @usage solid$support()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid support
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/support.svg}
#' \url{https://heroicons.dev}
#' @export
solid$support <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-support", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-2 0c0 .993-.241 1.929-.668 2.754l-1.524-1.525a3.997 3.997 0 00.078-2.183l1.562-1.562C15.802 8.249 16 9.1 16 10zm-5.165 3.913l1.58 1.58A5.98 5.98 0 0110 16a5.976 5.976 0 01-2.516-.552l1.562-1.562a4.006 4.006 0 001.789.027zm-4.677-2.796a4.002 4.002 0 01-.041-2.08l-.08.08-1.53-1.533A5.98 5.98 0 004 10c0 .954.223 1.856.619 2.657l1.54-1.54zm1.088-6.45A5.974 5.974 0 0110 4c.954 0 1.856.223 2.657.619l-1.54 1.54a4.002 4.002 0 00-2.346.033L7.246 4.668zM12 10a2 2 0 11-4 0 2 2 0 014 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'switch_horizontal
#' @name switch_horizontal
#' @return create a solid or outline SVG icon of a switch_horizontal
#' @usage solid$switch_horizontal()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid switch_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/switch_horizontal.svg}
#' \url{https://heroicons.dev}
#' @export
solid$switch_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-switch_horizontal", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M8 5a1 1 0 100 2h5.586l-1.293 1.293a1 1 0 001.414 1.414l3-3a1 1 0 000-1.414l-3-3a1 1 0 10-1.414 1.414L13.586 5H8zM12 15a1 1 0 100-2H6.414l1.293-1.293a1 1 0 10-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L6.414 15H12z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'switch_vertical
#' @name switch_vertical
#' @return create a solid or outline SVG icon of a switch_vertical
#' @usage solid$switch_vertical()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid switch_vertical
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/switch_vertical.svg}
#' \url{https://heroicons.dev}
#' @export
solid$switch_vertical <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-switch_vertical", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M5 12a1 1 0 102 0V6.414l1.293 1.293a1 1 0 001.414-1.414l-3-3a1 1 0 00-1.414 0l-3 3a1 1 0 001.414 1.414L5 6.414V12zM15 8a1 1 0 10-2 0v5.586l-1.293-1.293a1 1 0 00-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L15 13.586V8z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'tag
#' @name tag
#' @return create a solid or outline SVG icon of a tag
#' @usage solid$tag()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid tag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/tag.svg}
#' \url{https://heroicons.dev}
#' @export
solid$tag <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-tag", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M17.707 9.293a1 1 0 010 1.414l-7 7a1 1 0 01-1.414 0l-7-7A.997.997 0 012 10V5a3 3 0 013-3h5c.256 0 .512.098.707.293l7 7zM5 6a1 1 0 100-2 1 1 0 000 2z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'template
#' @name template
#' @return create a solid or outline SVG icon of a template
#' @usage solid$template()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid template
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/template.svg}
#' \url{https://heroicons.dev}
#' @export
solid$template <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-template", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M3 4a1 1 0 011-1h12a1 1 0 011 1v2a1 1 0 01-1 1H4a1 1 0 01-1-1V4zM3 10a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H4a1 1 0 01-1-1v-6zM14 9a1 1 0 00-1 1v6a1 1 0 001 1h2a1 1 0 001-1v-6a1 1 0 00-1-1h-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'terminal
#' @name terminal
#' @return create a solid or outline SVG icon of a terminal
#' @usage solid$terminal()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid terminal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/terminal.svg}
#' \url{https://heroicons.dev}
#' @export
solid$terminal <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-terminal", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M2 5a2 2 0 012-2h12a2 2 0 012 2v10a2 2 0 01-2 2H4a2 2 0 01-2-2V5zm3.293 1.293a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 01-1.414-1.414L7.586 10 5.293 7.707a1 1 0 010-1.414zM11 12a1 1 0 100 2h3a1 1 0 100-2h-3z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'thumb_down
#' @name thumb_down
#' @return create a solid or outline SVG icon of a thumb_down
#' @usage solid$thumb_down()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid thumb_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/thumb_down.svg}
#' \url{https://heroicons.dev}
#' @export
solid$thumb_down <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-thumb_down", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M18 9.5a1.5 1.5 0 11-3 0v-6a1.5 1.5 0 013 0v6zM14 9.667v-5.43a2 2 0 00-1.105-1.79l-.05-.025A4 4 0 0011.055 2H5.64a2 2 0 00-1.962 1.608l-1.2 6A2 2 0 004.44 12H8v4a2 2 0 002 2 1 1 0 001-1v-.667a4 4 0 01.8-2.4l1.4-1.866a4 4 0 00.8-2.4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'thumb_up
#' @name thumb_up
#' @return create a solid or outline SVG icon of a thumb_up
#' @usage solid$thumb_up()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid thumb_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/thumb_up.svg}
#' \url{https://heroicons.dev}
#' @export
solid$thumb_up <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-thumb_up", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 10.5a1.5 1.5 0 113 0v6a1.5 1.5 0 01-3 0v-6zM6 10.333v5.43a2 2 0 001.106 1.79l.05.025A4 4 0 008.943 18h5.416a2 2 0 001.962-1.608l1.2-6A2 2 0 0015.56 8H12V4a2 2 0 00-2-2 1 1 0 00-1 1v.667a4 4 0 01-.8 2.4L6.8 7.933a4 4 0 00-.8 2.4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'ticket
#' @name ticket
#' @return create a solid or outline SVG icon of a ticket
#' @usage solid$ticket()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid ticket
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/ticket.svg}
#' \url{https://heroicons.dev}
#' @export
solid$ticket <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-ticket", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h12a2 2 0 012 2v2a2 2 0 100 4v2a2 2 0 01-2 2H4a2 2 0 01-2-2v-2a2 2 0 100-4V6z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'translate
#' @name translate
#' @return create a solid or outline SVG icon of a translate
#' @usage solid$translate()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid translate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/translate.svg}
#' \url{https://heroicons.dev}
#' @export
solid$translate <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-translate", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M7 2a1 1 0 011 1v1h3a1 1 0 110 2H9.578a18.87 18.87 0 01-1.724 4.78c.29.354.596.696.914 1.026a1 1 0 11-1.44 1.389c-.188-.196-.373-.396-.554-.6a19.098 19.098 0 01-3.107 3.567 1 1 0 01-1.334-1.49 17.087 17.087 0 003.13-3.733 18.992 18.992 0 01-1.487-2.494 1 1 0 111.79-.89c.234.47.489.928.764 1.372.417-.934.752-1.913.997-2.927H3a1 1 0 110-2h3V3a1 1 0 011-1zm6 6a1 1 0 01.894.553l2.991 5.982a.869.869 0 01.02.037l.99 1.98a1 1 0 11-1.79.895L15.383 16h-4.764l-.724 1.447a1 1 0 11-1.788-.894l.99-1.98.019-.038 2.99-5.982A1 1 0 0113 8zm-1.382 6h2.764L13 11.236 11.618 14z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'trash
#' @name trash
#' @return create a solid or outline SVG icon of a trash
#' @usage solid$trash()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid trash
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/trash.svg}
#' \url{https://heroicons.dev}
#' @export
solid$trash <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-trash", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'trending_down
#' @name trending_down
#' @return create a solid or outline SVG icon of a trending_down
#' @usage solid$trending_down()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid trending_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/trending_down.svg}
#' \url{https://heroicons.dev}
#' @export
solid$trending_down <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-trending_down", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M12 13a1 1 0 100 2h5a1 1 0 001-1V9a1 1 0 10-2 0v2.586l-4.293-4.293a1 1 0 00-1.414 0L8 9.586 3.707 5.293a1 1 0 00-1.414 1.414l5 5a1 1 0 001.414 0L11 9.414 14.586 13H12z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'trending_up
#' @name trending_up
#' @return create a solid or outline SVG icon of a trending_up
#' @usage solid$trending_up()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid trending_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/trending_up.svg}
#' \url{https://heroicons.dev}
#' @export
solid$trending_up <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-trending_up", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M12 7a1 1 0 110-2h5a1 1 0 011 1v5a1 1 0 11-2 0V8.414l-4.293 4.293a1 1 0 01-1.414 0L8 10.414l-4.293 4.293a1 1 0 01-1.414-1.414l5-5a1 1 0 011.414 0L11 10.586 14.586 7H12z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'upload
#' @name upload
#' @return create a solid or outline SVG icon of a upload
#' @usage solid$upload()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid upload
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/upload.svg}
#' \url{https://heroicons.dev}
#' @export
solid$upload <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-upload", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 17a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM6.293 6.707a1 1 0 010-1.414l3-3a1 1 0 011.414 0l3 3a1 1 0 01-1.414 1.414L11 5.414V13a1 1 0 11-2 0V5.414L7.707 6.707a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'user_add
#' @name user_add
#' @return create a solid or outline SVG icon of a user_add
#' @usage solid$user_add()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid user_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_add.svg}
#' \url{https://heroicons.dev}
#' @export
solid$user_add <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-user_add", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M8 9a3 3 0 100-6 3 3 0 000 6zM8 11a6 6 0 016 6H2a6 6 0 016-6zM16 7a1 1 0 10-2 0v1h-1a1 1 0 100 2h1v1a1 1 0 102 0v-1h1a1 1 0 100-2h-1V7z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'user_circle
#' @name user_circle
#' @return create a solid or outline SVG icon of a user_circle
#' @usage solid$user_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid user_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$user_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-user_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-6-3a2 2 0 11-4 0 2 2 0 014 0zm-2 4a5 5 0 00-4.546 2.916A5.986 5.986 0 0010 16a5.986 5.986 0 004.546-2.084A5 5 0 0010 11z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'user_group
#' @name user_group
#' @return create a solid or outline SVG icon of a user_group
#' @usage solid$user_group()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid user_group
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_group.svg}
#' \url{https://heroicons.dev}
#' @export
solid$user_group <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-user_group", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M13 6a3 3 0 11-6 0 3 3 0 016 0zM18 8a2 2 0 11-4 0 2 2 0 014 0zM14 15a4 4 0 00-8 0v3h8v-3zM6 8a2 2 0 11-4 0 2 2 0 014 0zM16 18v-3a5.972 5.972 0 00-.75-2.906A3.005 3.005 0 0119 15v3h-3zM4.75 12.094A5.973 5.973 0 004 15v3H1v-3a3 3 0 013.75-2.906z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'user_remove
#' @name user_remove
#' @return create a solid or outline SVG icon of a user_remove
#' @usage solid$user_remove()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid user_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_remove.svg}
#' \url{https://heroicons.dev}
#' @export
solid$user_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-user_remove", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M11 6a3 3 0 11-6 0 3 3 0 016 0zM14 17a6 6 0 00-12 0h12zM13 8a1 1 0 100 2h4a1 1 0 100-2h-4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'user
#' @name user
#' @return create a solid or outline SVG icon of a user
#' @usage solid$user()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid user
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user.svg}
#' \url{https://heroicons.dev}
#' @export
solid$user <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-user", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 9a3 3 0 100-6 3 3 0 000 6zm-7 9a7 7 0 1114 0H3z", `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'users
#' @name users
#' @return create a solid or outline SVG icon of a users
#' @usage solid$users()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid users
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/users.svg}
#' \url{https://heroicons.dev}
#' @export
solid$users <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-users", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M9 6a3 3 0 11-6 0 3 3 0 016 0zM17 6a3 3 0 11-6 0 3 3 0 016 0zM12.93 17c.046-.327.07-.66.07-1a6.97 6.97 0 00-1.5-4.33A5 5 0 0119 16v1h-6.07zM6 11a5 5 0 015 5v1H1v-1a5 5 0 015-5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'view_boards
#' @name view_boards
#' @return create a solid or outline SVG icon of a view_boards
#' @usage solid$view_boards()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid view_boards
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_boards.svg}
#' \url{https://heroicons.dev}
#' @export
solid$view_boards <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-view_boards", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M2 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1H3a1 1 0 01-1-1V4zM8 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1H9a1 1 0 01-1-1V4zM15 3a1 1 0 00-1 1v12a1 1 0 001 1h2a1 1 0 001-1V4a1 1 0 00-1-1h-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'view_grid_add
#' @name view_grid_add
#' @return create a solid or outline SVG icon of a view_grid_add
#' @usage solid$view_grid_add()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid view_grid_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_grid_add.svg}
#' \url{https://heroicons.dev}
#' @export
solid$view_grid_add <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-view_grid_add", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM14 11a1 1 0 011 1v1h1a1 1 0 110 2h-1v1a1 1 0 11-2 0v-1h-1a1 1 0 110-2h1v-1a1 1 0 011-1z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'view_grid
#' @name view_grid
#' @return create a solid or outline SVG icon of a view_grid
#' @usage solid$view_grid()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid view_grid
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_grid.svg}
#' \url{https://heroicons.dev}
#' @export
solid$view_grid <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-view_grid", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM11 13a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'view_list
#' @name view_list
#' @return create a solid or outline SVG icon of a view_list
#' @usage solid$view_list()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid view_list
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_list.svg}
#' \url{https://heroicons.dev}
#' @export
solid$view_list <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-view_list", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M3 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm0 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm0 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm0 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'volume_off
#' @name volume_off
#' @return create a solid or outline SVG icon of a volume_off
#' @usage solid$volume_off()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid volume_off
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/volume_off.svg}
#' \url{https://heroicons.dev}
#' @export
solid$volume_off <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-volume_off", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M9.383 3.076A1 1 0 0110 4v12a1 1 0 01-1.707.707L4.586 13H2a1 1 0 01-1-1V8a1 1 0 011-1h2.586l3.707-3.707a1 1 0 011.09-.217zM12.293 7.293a1 1 0 011.414 0L15 8.586l1.293-1.293a1 1 0 111.414 1.414L16.414 10l1.293 1.293a1 1 0 01-1.414 1.414L15 11.414l-1.293 1.293a1 1 0 01-1.414-1.414L13.586 10l-1.293-1.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'volume_up
#' @name volume_up
#' @return create a solid or outline SVG icon of a volume_up
#' @usage solid$volume_up()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid volume_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/volume_up.svg}
#' \url{https://heroicons.dev}
#' @export
solid$volume_up <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-volume_up", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M9.383 3.076A1 1 0 0110 4v12a1 1 0 01-1.707.707L4.586 13H2a1 1 0 01-1-1V8a1 1 0 011-1h2.586l3.707-3.707a1 1 0 011.09-.217zM14.657 2.929a1 1 0 011.414 0A9.972 9.972 0 0119 10a9.972 9.972 0 01-2.929 7.071 1 1 0 01-1.414-1.414A7.971 7.971 0 0017 10c0-2.21-.894-4.208-2.343-5.657a1 1 0 010-1.414zm-2.829 2.828a1 1 0 011.415 0A5.983 5.983 0 0115 10a5.984 5.984 0 01-1.757 4.243 1 1 0 01-1.415-1.415A3.984 3.984 0 0013 10a3.983 3.983 0 00-1.172-2.828 1 1 0 010-1.415z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'x_circle
#' @name x_circle
#' @return create a solid or outline SVG icon of a x_circle
#' @usage solid$x_circle()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid x_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/x_circle.svg}
#' \url{https://heroicons.dev}
#' @export
solid$x_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-x_circle", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'x
#' @name x
#' @return create a solid or outline SVG icon of a x
#' @usage solid$x()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid x
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/x.svg}
#' \url{https://heroicons.dev}
#' @export
solid$x <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-x", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'zoom_in
#' @name zoom_in
#' @return create a solid or outline SVG icon of a zoom_in
#' @usage solid$zoom_in()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid zoom_in
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/zoom_in.svg}
#' \url{https://heroicons.dev}
#' @export
solid$zoom_in <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-zoom_in", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(d = "M5 8a1 1 0 011-1h1V6a1 1 0 012 0v1h1a1 1 0 110 2H9v1a1 1 0 11-2 0V9H6a1 1 0 01-1-1z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8zm6-4a4 4 0 100 8 4 4 0 000-8z", 
            `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

#'zoom_out
#' @name zoom_out
#' @return create a solid or outline SVG icon of a zoom_out
#' @usage solid$zoom_out()
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @keywords rheroicons solid zoom_out
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/zoom_out.svg}
#' \url{https://heroicons.dev}
#' @export
solid$zoom_out <- function(id = NULL, class = NULL, aria_hidden = FALSE) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons-solid rheroicons-zoom_out", 
        aria_hidden = aria_hidden, width = "20", height = "20", viewBox = "0 0 20 20", 
        fill = "currentColor", tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 8a1 1 0 011-1h4a1 1 0 110 2H6a1 1 0 01-1-1z", `clip-rule` = "evenodd"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    return(svg)
}

