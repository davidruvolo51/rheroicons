#' OUTLINE SVG Icons
#' @name outline
#' @keywords rheroicons outline
#' @return outline heroicons
#' @references
#' \url{https://github.com/refactoringui/heroicons}
#' \url{https://davidruvolo.shinyapps.io/rheroicons-demo/}
#' @examples
#' rheroicons::outline$book_open()
#' rheroicons::outline$book_open(id = 'myBookIcon')
#' rheroicons::outline$book_open(class = 'my-icon-set')
#' rheroicons::outline$book_open(aria_hidden = FALSE, title = 'read document')
#' @importFrom htmltools tag
#' @export
outline <- list()

#' adjustments
#' @name adjustments
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''adjustments'
#' @keywords rheroicons outline adjustments
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/adjustments.svg}
#' @examples
#' rheroicons::outline$adjustments(
#'   id = 'my_adjustments_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the adjustments icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$adjustments <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_adjustments", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' annotation
#' @name annotation
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''annotation'
#' @keywords rheroicons outline annotation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/annotation.svg}
#' @examples
#' rheroicons::outline$annotation(
#'   id = 'my_annotation_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the annotation icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$annotation <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_annotation", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 8h10M7 12h4m1 8l-4-4H5a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v8a2 2 0 01-2 2h-3l-4 4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' archive
#' @name archive
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''archive'
#' @keywords rheroicons outline archive
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/archive.svg}
#' @examples
#' rheroicons::outline$archive(
#'   id = 'my_archive_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the archive icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$archive <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_archive", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 8h14M5 8a2 2 0 110-4h14a2 2 0 110 4M5 8v10a2 2 0 002 2h10a2 2 0 002-2V8m-9 4h4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_circle_down
#' @name arrow_circle_down
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_circle_down'
#' @keywords rheroicons outline arrow_circle_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_circle_down.svg}
#' @examples
#' rheroicons::outline$arrow_circle_down(
#'   id = 'my_arrow_circle_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_down icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_circle_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_circle_down", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 13l-3 3m0 0l-3-3m3 3V8m0 13a9 9 0 110-18 9 9 0 010 18z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_circle_left
#' @name arrow_circle_left
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_circle_left'
#' @keywords rheroicons outline arrow_circle_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_circle_left.svg}
#' @examples
#' rheroicons::outline$arrow_circle_left(
#'   id = 'my_arrow_circle_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_left icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_circle_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_circle_left", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M11 15l-3-3m0 0l3-3m-3 3h8M3 12a9 9 0 1118 0 9 9 0 01-18 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_circle_right
#' @name arrow_circle_right
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_circle_right'
#' @keywords rheroicons outline arrow_circle_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_circle_right.svg}
#' @examples
#' rheroicons::outline$arrow_circle_right(
#'   id = 'my_arrow_circle_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_right icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_circle_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_circle_right", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13 9l3 3m0 0l-3 3m3-3H8m13 0a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_circle_up
#' @name arrow_circle_up
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_circle_up'
#' @keywords rheroicons outline arrow_circle_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_circle_up.svg}
#' @examples
#' rheroicons::outline$arrow_circle_up(
#'   id = 'my_arrow_circle_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_up icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_circle_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_circle_up", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 11l3-3m0 0l3 3m-3-3v8m0-13a9 9 0 110 18 9 9 0 010-18z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_down
#' @name arrow_down
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_down'
#' @keywords rheroicons outline arrow_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_down.svg}
#' @examples
#' rheroicons::outline$arrow_down(
#'   id = 'my_arrow_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_down icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_down", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M19 14l-7 7m0 0l-7-7m7 7V3"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_left
#' @name arrow_left
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_left'
#' @keywords rheroicons outline arrow_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_left.svg}
#' @examples
#' rheroicons::outline$arrow_left(
#'   id = 'my_arrow_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_left icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_left", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M10 19l-7-7m0 0l7-7m-7 7h18"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_narrow_down
#' @name arrow_narrow_down
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_narrow_down'
#' @keywords rheroicons outline arrow_narrow_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_narrow_down.svg}
#' @examples
#' rheroicons::outline$arrow_narrow_down(
#'   id = 'my_arrow_narrow_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_down icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_narrow_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_narrow_down", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 17l-4 4m0 0l-4-4m4 4V3"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_narrow_left
#' @name arrow_narrow_left
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_narrow_left'
#' @keywords rheroicons outline arrow_narrow_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_narrow_left.svg}
#' @examples
#' rheroicons::outline$arrow_narrow_left(
#'   id = 'my_arrow_narrow_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_left icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_narrow_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_narrow_left", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 16l-4-4m0 0l4-4m-4 4h18"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_narrow_right
#' @name arrow_narrow_right
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_narrow_right'
#' @keywords rheroicons outline arrow_narrow_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_narrow_right.svg}
#' @examples
#' rheroicons::outline$arrow_narrow_right(
#'   id = 'my_arrow_narrow_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_right icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_narrow_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_narrow_right", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17 8l4 4m0 0l-4 4m4-4H3"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_narrow_up
#' @name arrow_narrow_up
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_narrow_up'
#' @keywords rheroicons outline arrow_narrow_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_narrow_up.svg}
#' @examples
#' rheroicons::outline$arrow_narrow_up(
#'   id = 'my_arrow_narrow_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_up icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_narrow_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_narrow_up", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 7l4-4m0 0l4 4m-4-4v18"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_right
#' @name arrow_right
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_right'
#' @keywords rheroicons outline arrow_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_right.svg}
#' @examples
#' rheroicons::outline$arrow_right(
#'   id = 'my_arrow_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_right icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_right", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M14 5l7 7m0 0l-7 7m7-7H3"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrow_up
#' @name arrow_up
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrow_up'
#' @keywords rheroicons outline arrow_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrow_up.svg}
#' @examples
#' rheroicons::outline$arrow_up(
#'   id = 'my_arrow_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_up icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrow_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrow_up", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 10l7-7m0 0l7 7m-7-7v18"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' arrows_expand
#' @name arrows_expand
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''arrows_expand'
#' @keywords rheroicons outline arrows_expand
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/arrows_expand.svg}
#' @examples
#' rheroicons::outline$arrows_expand(
#'   id = 'my_arrows_expand_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrows_expand icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$arrows_expand <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_arrows_expand", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 8V4m0 0h4M4 4l5 5m11-1V4m0 0h-4m4 0l-5 5M4 16v4m0 0h4m-4 0l5-5m11 5l-5-5m5 5v-4m0 4h-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' at_symbol
#' @name at_symbol
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''at_symbol'
#' @keywords rheroicons outline at_symbol
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/at_symbol.svg}
#' @examples
#' rheroicons::outline$at_symbol(
#'   id = 'my_at_symbol_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the at_symbol icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$at_symbol <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_at_symbol", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 12a4 4 0 10-8 0 4 4 0 008 0zm0 0v1.5a2.5 2.5 0 005 0V12a9 9 0 10-9 9m4.5-1.206a8.959 8.959 0 01-4.5 1.207"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' badge_check
#' @name badge_check
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''badge_check'
#' @keywords rheroicons outline badge_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/badge_check.svg}
#' @examples
#' rheroicons::outline$badge_check(
#'   id = 'my_badge_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the badge_check icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$badge_check <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_badge_check", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 12l2 2 4-4M7.835 4.697a3.42 3.42 0 001.946-.806 3.42 3.42 0 014.438 0 3.42 3.42 0 001.946.806 3.42 3.42 0 013.138 3.138 3.42 3.42 0 00.806 1.946 3.42 3.42 0 010 4.438 3.42 3.42 0 00-.806 1.946 3.42 3.42 0 01-3.138 3.138 3.42 3.42 0 00-1.946.806 3.42 3.42 0 01-4.438 0 3.42 3.42 0 00-1.946-.806 3.42 3.42 0 01-3.138-3.138 3.42 3.42 0 00-.806-1.946 3.42 3.42 0 010-4.438 3.42 3.42 0 00.806-1.946 3.42 3.42 0 013.138-3.138z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' ban
#' @name ban
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''ban'
#' @keywords rheroicons outline ban
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/ban.svg}
#' @examples
#' rheroicons::outline$ban(
#'   id = 'my_ban_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the ban icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$ban <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_ban", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M18.364 18.364A9 9 0 005.636 5.636m12.728 12.728A9 9 0 015.636 5.636m12.728 12.728L5.636 5.636"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' bell
#' @name bell
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''bell'
#' @keywords rheroicons outline bell
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/bell.svg}
#' @examples
#' rheroicons::outline$bell(
#'   id = 'my_bell_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the bell icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$bell <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_bell", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' book_open
#' @name book_open
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''book_open'
#' @keywords rheroicons outline book_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/book_open.svg}
#' @examples
#' rheroicons::outline$book_open(
#'   id = 'my_book_open_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the book_open icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$book_open <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_book_open", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 6.253v13m0-13C10.832 5.477 9.246 5 7.5 5S4.168 5.477 3 6.253v13C4.168 18.477 5.754 18 7.5 18s3.332.477 4.5 1.253m0-13C13.168 5.477 14.754 5 16.5 5c1.747 0 3.332.477 4.5 1.253v13C19.832 18.477 18.247 18 16.5 18c-1.746 0-3.332.477-4.5 1.253"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' bookmark_alt
#' @name bookmark_alt
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''bookmark_alt'
#' @keywords rheroicons outline bookmark_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/bookmark_alt.svg}
#' @examples
#' rheroicons::outline$bookmark_alt(
#'   id = 'my_bookmark_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the bookmark_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$bookmark_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_bookmark_alt", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 4v12l-4-2-4 2V4M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' bookmark
#' @name bookmark
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''bookmark'
#' @keywords rheroicons outline bookmark
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/bookmark.svg}
#' @examples
#' rheroicons::outline$bookmark(
#'   id = 'my_bookmark_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the bookmark icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$bookmark <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_bookmark", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' briefcase
#' @name briefcase
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''briefcase'
#' @keywords rheroicons outline briefcase
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/briefcase.svg}
#' @examples
#' rheroicons::outline$briefcase(
#'   id = 'my_briefcase_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the briefcase icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$briefcase <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_briefcase", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 13.255A23.931 23.931 0 0112 15c-3.183 0-6.22-.62-9-1.745M16 6V4a2 2 0 00-2-2h-4a2 2 0 00-2 2v2m4 6h.01M5 20h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' calendar
#' @name calendar
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''calendar'
#' @keywords rheroicons outline calendar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/calendar.svg}
#' @examples
#' rheroicons::outline$calendar(
#'   id = 'my_calendar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the calendar icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$calendar <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_calendar", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' camera
#' @name camera
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''camera'
#' @keywords rheroicons outline camera
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/camera.svg}
#' @examples
#' rheroicons::outline$camera(
#'   id = 'my_camera_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the camera icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$camera <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_camera", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 9a2 2 0 012-2h.93a2 2 0 001.664-.89l.812-1.22A2 2 0 0110.07 4h3.86a2 2 0 011.664.89l.812 1.22A2 2 0 0018.07 7H19a2 2 0 012 2v9a2 2 0 01-2 2H5a2 2 0 01-2-2V9z")), 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 13a3 3 0 11-6 0 3 3 0 016 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' cash
#' @name cash
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''cash'
#' @keywords rheroicons outline cash
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/cash.svg}
#' @examples
#' rheroicons::outline$cash(
#'   id = 'my_cash_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cash icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$cash <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_cash", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17 9V7a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2m2 4h10a2 2 0 002-2v-6a2 2 0 00-2-2H9a2 2 0 00-2 2v6a2 2 0 002 2zm7-5a2 2 0 11-4 0 2 2 0 014 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chart_bar
#' @name chart_bar
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chart_bar'
#' @keywords rheroicons outline chart_bar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chart_bar.svg}
#' @examples
#' rheroicons::outline$chart_bar(
#'   id = 'my_chart_bar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chart_bar icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chart_bar <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chart_bar", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chart_pie
#' @name chart_pie
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chart_pie'
#' @keywords rheroicons outline chart_pie
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chart_pie.svg}
#' @examples
#' rheroicons::outline$chart_pie(
#'   id = 'my_chart_pie_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chart_pie icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chart_pie <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chart_pie", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M11 3.055A9.001 9.001 0 1020.945 13H11V3.055z")), 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M20.488 9H15V3.512A9.025 9.025 0 0120.488 9z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chart_square_bar
#' @name chart_square_bar
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chart_square_bar'
#' @keywords rheroicons outline chart_square_bar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chart_square_bar.svg}
#' @examples
#' rheroicons::outline$chart_square_bar(
#'   id = 'my_chart_square_bar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chart_square_bar icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chart_square_bar <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chart_square_bar", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 8v8m-4-5v5m-4-2v2m-2 4h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chat_alt_2
#' @name chat_alt_2
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chat_alt_2'
#' @keywords rheroicons outline chat_alt_2
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chat_alt_2.svg}
#' @examples
#' rheroicons::outline$chat_alt_2(
#'   id = 'my_chat_alt_2_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chat_alt_2 icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chat_alt_2 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chat_alt_2", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17 8h2a2 2 0 012 2v6a2 2 0 01-2 2h-2v4l-4-4H9a1.994 1.994 0 01-1.414-.586m0 0L11 14h4a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2v4l.586-.586z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chat_alt
#' @name chat_alt
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chat_alt'
#' @keywords rheroicons outline chat_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chat_alt.svg}
#' @examples
#' rheroicons::outline$chat_alt(
#'   id = 'my_chat_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chat_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chat_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chat_alt", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 10h.01M12 10h.01M16 10h.01M9 16H5a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v8a2 2 0 01-2 2h-5l-5 5v-5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chat
#' @name chat
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chat'
#' @keywords rheroicons outline chat
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chat.svg}
#' @examples
#' rheroicons::outline$chat(
#'   id = 'my_chat_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chat icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chat <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chat", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' check_circle
#' @name check_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''check_circle'
#' @keywords rheroicons outline check_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/check_circle.svg}
#' @examples
#' rheroicons::outline$check_circle(
#'   id = 'my_check_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the check_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$check_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_check_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' check
#' @name check
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''check'
#' @keywords rheroicons outline check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/check.svg}
#' @examples
#' rheroicons::outline$check(
#'   id = 'my_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the check icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$check <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_check", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 13l4 4L19 7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chevron_down
#' @name chevron_down
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chevron_down'
#' @keywords rheroicons outline chevron_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chevron_down.svg}
#' @examples
#' rheroicons::outline$chevron_down(
#'   id = 'my_chevron_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_down icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chevron_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chevron_down", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M19 9l-7 7-7-7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chevron_left
#' @name chevron_left
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chevron_left'
#' @keywords rheroicons outline chevron_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chevron_left.svg}
#' @examples
#' rheroicons::outline$chevron_left(
#'   id = 'my_chevron_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_left icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chevron_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chevron_left", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 19l-7-7 7-7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chevron_right
#' @name chevron_right
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chevron_right'
#' @keywords rheroicons outline chevron_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chevron_right.svg}
#' @examples
#' rheroicons::outline$chevron_right(
#'   id = 'my_chevron_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_right icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chevron_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chevron_right", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 5l7 7-7 7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' chevron_up
#' @name chevron_up
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''chevron_up'
#' @keywords rheroicons outline chevron_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/chevron_up.svg}
#' @examples
#' rheroicons::outline$chevron_up(
#'   id = 'my_chevron_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_up icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$chevron_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_chevron_up", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 15l7-7 7 7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' clipboard_check
#' @name clipboard_check
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''clipboard_check'
#' @keywords rheroicons outline clipboard_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/clipboard_check.svg}
#' @examples
#' rheroicons::outline$clipboard_check(
#'   id = 'my_clipboard_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard_check icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$clipboard_check <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_clipboard_check", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' clipboard_copy
#' @name clipboard_copy
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''clipboard_copy'
#' @keywords rheroicons outline clipboard_copy
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/clipboard_copy.svg}
#' @examples
#' rheroicons::outline$clipboard_copy(
#'   id = 'my_clipboard_copy_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard_copy icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$clipboard_copy <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_clipboard_copy", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 5H6a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2v-1M8 5a2 2 0 002 2h2a2 2 0 002-2M8 5a2 2 0 012-2h2a2 2 0 012 2m0 0h2a2 2 0 012 2v3m2 4H10m0 0l3-3m-3 3l3 3"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' clipboard_list
#' @name clipboard_list
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''clipboard_list'
#' @keywords rheroicons outline clipboard_list
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/clipboard_list.svg}
#' @examples
#' rheroicons::outline$clipboard_list(
#'   id = 'my_clipboard_list_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard_list icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$clipboard_list <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_clipboard_list", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-3 7h3m-3 4h3m-6-4h.01M9 16h.01"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' clipboard
#' @name clipboard
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''clipboard'
#' @keywords rheroicons outline clipboard
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/clipboard.svg}
#' @examples
#' rheroicons::outline$clipboard(
#'   id = 'my_clipboard_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$clipboard <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_clipboard", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' clock
#' @name clock
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''clock'
#' @keywords rheroicons outline clock
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/clock.svg}
#' @examples
#' rheroicons::outline$clock(
#'   id = 'my_clock_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clock icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$clock <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_clock", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' cloud_download
#' @name cloud_download
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''cloud_download'
#' @keywords rheroicons outline cloud_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/cloud_download.svg}
#' @examples
#' rheroicons::outline$cloud_download(
#'   id = 'my_cloud_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cloud_download icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$cloud_download <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_cloud_download", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 16a5 5 0 01-.916-9.916 5.002 5.002 0 019.832 0A5.002 5.002 0 0116 16m-7 3l3 3m0 0l3-3m-3 3V10"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' cloud_upload
#' @name cloud_upload
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''cloud_upload'
#' @keywords rheroicons outline cloud_upload
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/cloud_upload.svg}
#' @examples
#' rheroicons::outline$cloud_upload(
#'   id = 'my_cloud_upload_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cloud_upload icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$cloud_upload <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_cloud_upload", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 17a5 5 0 01-.916-9.916 5.002 5.002 0 019.832 0A5.002 5.002 0 0116 17m-7-5l3-3m0 0l3 3m-3-3v12"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' code
#' @name code
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''code'
#' @keywords rheroicons outline code
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/code.svg}
#' @examples
#' rheroicons::outline$code(
#'   id = 'my_code_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the code icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$code <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_code", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' cog
#' @name cog
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''cog'
#' @keywords rheroicons outline cog
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/cog.svg}
#' @examples
#' rheroicons::outline$cog(
#'   id = 'my_cog_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cog icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$cog <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_cog", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z")), 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 12a3 3 0 11-6 0 3 3 0 016 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' collection
#' @name collection
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''collection'
#' @keywords rheroicons outline collection
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/collection.svg}
#' @examples
#' rheroicons::outline$collection(
#'   id = 'my_collection_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the collection icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$collection <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_collection", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' color_swatch
#' @name color_swatch
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''color_swatch'
#' @keywords rheroicons outline color_swatch
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/color_swatch.svg}
#' @examples
#' rheroicons::outline$color_swatch(
#'   id = 'my_color_swatch_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the color_swatch icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$color_swatch <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_color_swatch", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 21a4 4 0 01-4-4V5a2 2 0 012-2h4a2 2 0 012 2v12a4 4 0 01-4 4zm0 0h12a2 2 0 002-2v-4a2 2 0 00-2-2h-2.343M11 7.343l1.657-1.657a2 2 0 012.828 0l2.829 2.829a2 2 0 010 2.828l-8.486 8.485M7 17h.01"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' credit_card
#' @name credit_card
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''credit_card'
#' @keywords rheroicons outline credit_card
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/credit_card.svg}
#' @examples
#' rheroicons::outline$credit_card(
#'   id = 'my_credit_card_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the credit_card icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$credit_card <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_credit_card", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' currency_dollar
#' @name currency_dollar
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''currency_dollar'
#' @keywords rheroicons outline currency_dollar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/currency_dollar.svg}
#' @examples
#' rheroicons::outline$currency_dollar(
#'   id = 'my_currency_dollar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_dollar icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$currency_dollar <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_currency_dollar", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' currency_euro
#' @name currency_euro
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''currency_euro'
#' @keywords rheroicons outline currency_euro
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/currency_euro.svg}
#' @examples
#' rheroicons::outline$currency_euro(
#'   id = 'my_currency_euro_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_euro icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$currency_euro <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_currency_euro", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M14.121 15.536c-1.171 1.952-3.07 1.952-4.242 0-1.172-1.953-1.172-5.119 0-7.072 1.171-1.952 3.07-1.952 4.242 0M8 10.5h4m-4 3h4m9-1.5a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' currency_pound
#' @name currency_pound
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''currency_pound'
#' @keywords rheroicons outline currency_pound
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/currency_pound.svg}
#' @examples
#' rheroicons::outline$currency_pound(
#'   id = 'my_currency_pound_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_pound icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$currency_pound <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_currency_pound", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 9a2 2 0 10-4 0v5a2 2 0 01-2 2h6m-6-4h4m8 0a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' currency_rupee
#' @name currency_rupee
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''currency_rupee'
#' @keywords rheroicons outline currency_rupee
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/currency_rupee.svg}
#' @examples
#' rheroicons::outline$currency_rupee(
#'   id = 'my_currency_rupee_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_rupee icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$currency_rupee <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_currency_rupee", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 8h6m-5 0a3 3 0 110 6H9l3 3m-3-6h6m6 1a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' currency_yen
#' @name currency_yen
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''currency_yen'
#' @keywords rheroicons outline currency_yen
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/currency_yen.svg}
#' @examples
#' rheroicons::outline$currency_yen(
#'   id = 'my_currency_yen_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_yen icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$currency_yen <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_currency_yen", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 8l3 5m0 0l3-5m-3 5v4m-3-5h6m-6 3h6m6-3a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' cursor_click
#' @name cursor_click
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''cursor_click'
#' @keywords rheroicons outline cursor_click
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/cursor_click.svg}
#' @examples
#' rheroicons::outline$cursor_click(
#'   id = 'my_cursor_click_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cursor_click icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$cursor_click <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_cursor_click", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 15l-2 5L9 9l11 4-5 2zm0 0l5 5M7.188 2.239l.777 2.897M5.136 7.965l-2.898-.777M13.95 4.05l-2.122 2.122m-5.657 5.656l-2.12 2.122"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' desktop_computer
#' @name desktop_computer
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''desktop_computer'
#' @keywords rheroicons outline desktop_computer
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/desktop_computer.svg}
#' @examples
#' rheroicons::outline$desktop_computer(
#'   id = 'my_desktop_computer_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the desktop_computer icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$desktop_computer <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_desktop_computer", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9.75 17L9 20l-1 1h8l-1-1-.75-3M3 13h18M5 17h14a2 2 0 002-2V5a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' document_add
#' @name document_add
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''document_add'
#' @keywords rheroicons outline document_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/document_add.svg}
#' @examples
#' rheroicons::outline$document_add(
#'   id = 'my_document_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_add icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$document_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_document_add", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 13h6m-3-3v6m5 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' document_download
#' @name document_download
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''document_download'
#' @keywords rheroicons outline document_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/document_download.svg}
#' @examples
#' rheroicons::outline$document_download(
#'   id = 'my_document_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_download icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$document_download <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_document_download", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' document_duplicate
#' @name document_duplicate
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''document_duplicate'
#' @keywords rheroicons outline document_duplicate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/document_duplicate.svg}
#' @examples
#' rheroicons::outline$document_duplicate(
#'   id = 'my_document_duplicate_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_duplicate icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$document_duplicate <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_document_duplicate", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 7v8a2 2 0 002 2h6M8 7V5a2 2 0 012-2h4.586a1 1 0 01.707.293l4.414 4.414a1 1 0 01.293.707V15a2 2 0 01-2 2h-2M8 7H6a2 2 0 00-2 2v10a2 2 0 002 2h8a2 2 0 002-2v-2"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' document_remove
#' @name document_remove
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''document_remove'
#' @keywords rheroicons outline document_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/document_remove.svg}
#' @examples
#' rheroicons::outline$document_remove(
#'   id = 'my_document_remove_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_remove icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$document_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_document_remove", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 13h6m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' document_report
#' @name document_report
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''document_report'
#' @keywords rheroicons outline document_report
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/document_report.svg}
#' @examples
#' rheroicons::outline$document_report(
#'   id = 'my_document_report_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_report icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$document_report <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_document_report", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 17v-2m3 2v-4m3 4v-6m2 10H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' document
#' @name document
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''document'
#' @keywords rheroicons outline document
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/document.svg}
#' @examples
#' rheroicons::outline$document(
#'   id = 'my_document_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$document <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_document", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 21h10a2 2 0 002-2V9.414a1 1 0 00-.293-.707l-5.414-5.414A1 1 0 0012.586 3H7a2 2 0 00-2 2v14a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' dots_circle_horizontal
#' @name dots_circle_horizontal
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''dots_circle_horizontal'
#' @keywords rheroicons outline dots_circle_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/dots_circle_horizontal.svg}
#' @examples
#' rheroicons::outline$dots_circle_horizontal(
#'   id = 'my_dots_circle_horizontal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the dots_circle_horizontal icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$dots_circle_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_dots_circle_horizontal", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 12h.01M12 12h.01M16 12h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' dots_horizontal
#' @name dots_horizontal
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''dots_horizontal'
#' @keywords rheroicons outline dots_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/dots_horizontal.svg}
#' @examples
#' rheroicons::outline$dots_horizontal(
#'   id = 'my_dots_horizontal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the dots_horizontal icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$dots_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_dots_horizontal", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 12h.01M12 12h.01M19 12h.01M6 12a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0zm7 0a1 1 0 11-2 0 1 1 0 012 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' dots_vertical
#' @name dots_vertical
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''dots_vertical'
#' @keywords rheroicons outline dots_vertical
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/dots_vertical.svg}
#' @examples
#' rheroicons::outline$dots_vertical(
#'   id = 'my_dots_vertical_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the dots_vertical icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$dots_vertical <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_dots_vertical", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 5v.01M12 12v.01M12 19v.01M12 6a1 1 0 110-2 1 1 0 010 2zm0 7a1 1 0 110-2 1 1 0 010 2zm0 7a1 1 0 110-2 1 1 0 010 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' download
#' @name download
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''download'
#' @keywords rheroicons outline download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/download.svg}
#' @examples
#' rheroicons::outline$download(
#'   id = 'my_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the download icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$download <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_download", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' duplicate
#' @name duplicate
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''duplicate'
#' @keywords rheroicons outline duplicate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/duplicate.svg}
#' @examples
#' rheroicons::outline$duplicate(
#'   id = 'my_duplicate_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the duplicate icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$duplicate <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_duplicate", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' emoji_happy
#' @name emoji_happy
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''emoji_happy'
#' @keywords rheroicons outline emoji_happy
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/emoji_happy.svg}
#' @examples
#' rheroicons::outline$emoji_happy(
#'   id = 'my_emoji_happy_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the emoji_happy icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$emoji_happy <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_emoji_happy", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M14.828 14.828a4 4 0 01-5.656 0M9 10h.01M15 10h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' emoji_sad
#' @name emoji_sad
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''emoji_sad'
#' @keywords rheroicons outline emoji_sad
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/emoji_sad.svg}
#' @examples
#' rheroicons::outline$emoji_sad(
#'   id = 'my_emoji_sad_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the emoji_sad icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$emoji_sad <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_emoji_sad", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9.172 16.172a4 4 0 015.656 0M9 10h.01M15 10h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' exclamation_circle
#' @name exclamation_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''exclamation_circle'
#' @keywords rheroicons outline exclamation_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/exclamation_circle.svg}
#' @examples
#' rheroicons::outline$exclamation_circle(
#'   id = 'my_exclamation_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the exclamation_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$exclamation_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_exclamation_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' exclamation
#' @name exclamation
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''exclamation'
#' @keywords rheroicons outline exclamation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/exclamation.svg}
#' @examples
#' rheroicons::outline$exclamation(
#'   id = 'my_exclamation_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the exclamation icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$exclamation <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_exclamation", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' external_link
#' @name external_link
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''external_link'
#' @keywords rheroicons outline external_link
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/external_link.svg}
#' @examples
#' rheroicons::outline$external_link(
#'   id = 'my_external_link_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the external_link icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$external_link <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_external_link", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' eye_off
#' @name eye_off
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''eye_off'
#' @keywords rheroicons outline eye_off
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/eye_off.svg}
#' @examples
#' rheroicons::outline$eye_off(
#'   id = 'my_eye_off_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the eye_off icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$eye_off <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_eye_off", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13.875 18.825A10.05 10.05 0 0112 19c-4.478 0-8.268-2.943-9.543-7a9.97 9.97 0 011.563-3.029m5.858.908a3 3 0 114.243 4.243M9.878 9.878l4.242 4.242M9.88 9.88l-3.29-3.29m7.532 7.532l3.29 3.29M3 3l3.59 3.59m0 0A9.953 9.953 0 0112 5c4.478 0 8.268 2.943 9.543 7a10.025 10.025 0 01-4.132 5.411m0 0L21 21"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' eye
#' @name eye
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''eye'
#' @keywords rheroicons outline eye
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/eye.svg}
#' @examples
#' rheroicons::outline$eye(
#'   id = 'my_eye_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the eye icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$eye <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_eye", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 12a3 3 0 11-6 0 3 3 0 016 0z")), tag(`_tag_name` = "path", 
            list(`stroke-linecap` = "round", `stroke-linejoin` = "round", `stroke-width` = "2", 
                d = "M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' filter
#' @name filter
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''filter'
#' @keywords rheroicons outline filter
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/filter.svg}
#' @examples
#' rheroicons::outline$filter(
#'   id = 'my_filter_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the filter icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$filter <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_filter", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 4a1 1 0 011-1h16a1 1 0 011 1v2.586a1 1 0 01-.293.707l-6.414 6.414a1 1 0 00-.293.707V17l-4 4v-6.586a1 1 0 00-.293-.707L3.293 7.293A1 1 0 013 6.586V4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' fire
#' @name fire
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''fire'
#' @keywords rheroicons outline fire
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/fire.svg}
#' @examples
#' rheroicons::outline$fire(
#'   id = 'my_fire_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the fire icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$fire <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_fire", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17.657 18.657A8 8 0 016.343 7.343S7 9 9 10c0-2 .5-5 2.986-7C14 5 16.09 5.777 17.656 7.343A7.975 7.975 0 0120 13a7.975 7.975 0 01-2.343 5.657z")), 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9.879 16.121A3 3 0 1012.015 11L11 14H9c0 .768.293 1.536.879 2.121z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' flag
#' @name flag
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''flag'
#' @keywords rheroicons outline flag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/flag.svg}
#' @examples
#' rheroicons::outline$flag(
#'   id = 'my_flag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the flag icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$flag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_flag", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 21v-4m0 0V5a2 2 0 012-2h6.5l1 1H21l-3 6 3 6h-8.5l-1-1H5a2 2 0 00-2 2zm9-13.5V9"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' folder_add
#' @name folder_add
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''folder_add'
#' @keywords rheroicons outline folder_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/folder_add.svg}
#' @examples
#' rheroicons::outline$folder_add(
#'   id = 'my_folder_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder_add icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$folder_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_folder_add", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 13h6m-3-3v6m-9 1V7a2 2 0 012-2h6l2 2h6a2 2 0 012 2v8a2 2 0 01-2 2H5a2 2 0 01-2-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' folder_download
#' @name folder_download
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''folder_download'
#' @keywords rheroicons outline folder_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/folder_download.svg}
#' @examples
#' rheroicons::outline$folder_download(
#'   id = 'my_folder_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder_download icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$folder_download <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_folder_download", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 10v6m0 0l-3-3m3 3l3-3M3 17V7a2 2 0 012-2h6l2 2h6a2 2 0 012 2v8a2 2 0 01-2 2H5a2 2 0 01-2-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' folder_remove
#' @name folder_remove
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''folder_remove'
#' @keywords rheroicons outline folder_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/folder_remove.svg}
#' @examples
#' rheroicons::outline$folder_remove(
#'   id = 'my_folder_remove_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder_remove icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$folder_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_folder_remove", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 13h6M3 17V7a2 2 0 012-2h6l2 2h6a2 2 0 012 2v8a2 2 0 01-2 2H5a2 2 0 01-2-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' folder
#' @name folder
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''folder'
#' @keywords rheroicons outline folder
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/folder.svg}
#' @examples
#' rheroicons::outline$folder(
#'   id = 'my_folder_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$folder <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_folder", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' globe_alt
#' @name globe_alt
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''globe_alt'
#' @keywords rheroicons outline globe_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/globe_alt.svg}
#' @examples
#' rheroicons::outline$globe_alt(
#'   id = 'my_globe_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the globe_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$globe_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_globe_alt", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 12a9 9 0 01-9 9m9-9a9 9 0 00-9-9m9 9H3m9 9a9 9 0 01-9-9m9 9c1.657 0 3-4.03 3-9s-1.343-9-3-9m0 18c-1.657 0-3-4.03-3-9s1.343-9 3-9m-9 9a9 9 0 019-9"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' globe
#' @name globe
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''globe'
#' @keywords rheroicons outline globe
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/globe.svg}
#' @examples
#' rheroicons::outline$globe(
#'   id = 'my_globe_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the globe icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$globe <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_globe", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' hand
#' @name hand
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''hand'
#' @keywords rheroicons outline hand
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/hand.svg}
#' @examples
#' rheroicons::outline$hand(
#'   id = 'my_hand_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the hand icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$hand <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_hand", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 11.5V14m0-2.5v-6a1.5 1.5 0 113 0m-3 6a1.5 1.5 0 00-3 0v2a7.5 7.5 0 0015 0v-5a1.5 1.5 0 00-3 0m-6-3V11m0-5.5v-1a1.5 1.5 0 013 0v1m0 0V11m0-5.5a1.5 1.5 0 013 0v3m0 0V11"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' hashtag
#' @name hashtag
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''hashtag'
#' @keywords rheroicons outline hashtag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/hashtag.svg}
#' @examples
#' rheroicons::outline$hashtag(
#'   id = 'my_hashtag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the hashtag icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$hashtag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_hashtag", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 20l4-16m2 16l4-16M6 9h14M4 15h14"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' heart
#' @name heart
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''heart'
#' @keywords rheroicons outline heart
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/heart.svg}
#' @examples
#' rheroicons::outline$heart(
#'   id = 'my_heart_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the heart icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$heart <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_heart", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4.318 6.318a4.5 4.5 0 000 6.364L12 20.364l7.682-7.682a4.5 4.5 0 00-6.364-6.364L12 7.636l-1.318-1.318a4.5 4.5 0 00-6.364 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' home
#' @name home
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''home'
#' @keywords rheroicons outline home
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/home.svg}
#' @examples
#' rheroicons::outline$home(
#'   id = 'my_home_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the home icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$home <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_home", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' inbox_in
#' @name inbox_in
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''inbox_in'
#' @keywords rheroicons outline inbox_in
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/inbox_in.svg}
#' @examples
#' rheroicons::outline$inbox_in(
#'   id = 'my_inbox_in_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the inbox_in icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$inbox_in <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_inbox_in", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 4H6a2 2 0 00-2 2v12a2 2 0 002 2h12a2 2 0 002-2V6a2 2 0 00-2-2h-2m-4-1v8m0 0l3-3m-3 3L9 8m-5 5h2.586a1 1 0 01.707.293l2.414 2.414a1 1 0 00.707.293h3.172a1 1 0 00.707-.293l2.414-2.414a1 1 0 01.707-.293H20"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' inbox
#' @name inbox
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''inbox'
#' @keywords rheroicons outline inbox
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/inbox.svg}
#' @examples
#' rheroicons::outline$inbox(
#'   id = 'my_inbox_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the inbox icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$inbox <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_inbox", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M20 13V6a2 2 0 00-2-2H6a2 2 0 00-2 2v7m16 0v5a2 2 0 01-2 2H6a2 2 0 01-2-2v-5m16 0h-2.586a1 1 0 00-.707.293l-2.414 2.414a1 1 0 01-.707.293h-3.172a1 1 0 01-.707-.293l-2.414-2.414A1 1 0 006.586 13H4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' information_circle
#' @name information_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''information_circle'
#' @keywords rheroicons outline information_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/information_circle.svg}
#' @examples
#' rheroicons::outline$information_circle(
#'   id = 'my_information_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the information_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$information_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_information_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' key
#' @name key
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''key'
#' @keywords rheroicons outline key
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/key.svg}
#' @examples
#' rheroicons::outline$key(
#'   id = 'my_key_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the key icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$key <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_key", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 7a2 2 0 012 2m4 0a6 6 0 01-7.743 5.743L11 17H9v2H7v2H4a1 1 0 01-1-1v-2.586a1 1 0 01.293-.707l5.964-5.964A6 6 0 1121 9z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' library
#' @name library
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''library'
#' @keywords rheroicons outline library
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/library.svg}
#' @examples
#' rheroicons::outline$library(
#'   id = 'my_library_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the library icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$library <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_library", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 14v3m4-3v3m4-3v3M3 21h18M3 10h18M3 7l9-4 9 4M4 10h16v11H4V10z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' light_bulb
#' @name light_bulb
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''light_bulb'
#' @keywords rheroicons outline light_bulb
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/light_bulb.svg}
#' @examples
#' rheroicons::outline$light_bulb(
#'   id = 'my_light_bulb_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the light_bulb icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$light_bulb <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_light_bulb", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' lightning_bolt
#' @name lightning_bolt
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''lightning_bolt'
#' @keywords rheroicons outline lightning_bolt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/lightning_bolt.svg}
#' @examples
#' rheroicons::outline$lightning_bolt(
#'   id = 'my_lightning_bolt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the lightning_bolt icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$lightning_bolt <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_lightning_bolt", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13 10V3L4 14h7v7l9-11h-7z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' link
#' @name link
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''link'
#' @keywords rheroicons outline link
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/link.svg}
#' @examples
#' rheroicons::outline$link(
#'   id = 'my_link_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the link icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$link <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_link", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13.828 10.172a4 4 0 00-5.656 0l-4 4a4 4 0 105.656 5.656l1.102-1.101m-.758-4.899a4 4 0 005.656 0l4-4a4 4 0 00-5.656-5.656l-1.1 1.1"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' location_marker
#' @name location_marker
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''location_marker'
#' @keywords rheroicons outline location_marker
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/location_marker.svg}
#' @examples
#' rheroicons::outline$location_marker(
#'   id = 'my_location_marker_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the location_marker icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$location_marker <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_location_marker", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17.657 16.657L13.414 20.9a1.998 1.998 0 01-2.827 0l-4.244-4.243a8 8 0 1111.314 0z")), 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 11a3 3 0 11-6 0 3 3 0 016 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' lock_closed
#' @name lock_closed
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''lock_closed'
#' @keywords rheroicons outline lock_closed
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/lock_closed.svg}
#' @examples
#' rheroicons::outline$lock_closed(
#'   id = 'my_lock_closed_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the lock_closed icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$lock_closed <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_lock_closed", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' lock_open
#' @name lock_open
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''lock_open'
#' @keywords rheroicons outline lock_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/lock_open.svg}
#' @examples
#' rheroicons::outline$lock_open(
#'   id = 'my_lock_open_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the lock_open icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$lock_open <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_lock_open", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 11V7a4 4 0 118 0m-4 8v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' logout
#' @name logout
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''logout'
#' @keywords rheroicons outline logout
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/logout.svg}
#' @examples
#' rheroicons::outline$logout(
#'   id = 'my_logout_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the logout icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$logout <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_logout", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M11 16l-4-4m0 0l4-4m-4 4h14m-5 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h7a3 3 0 013 3v1"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' mail_open
#' @name mail_open
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''mail_open'
#' @keywords rheroicons outline mail_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/mail_open.svg}
#' @examples
#' rheroicons::outline$mail_open(
#'   id = 'my_mail_open_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the mail_open icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$mail_open <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_mail_open", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 19v-8.93a2 2 0 01.89-1.664l7-4.666a2 2 0 012.22 0l7 4.666A2 2 0 0121 10.07V19M3 19a2 2 0 002 2h14a2 2 0 002-2M3 19l6.75-4.5M21 19l-6.75-4.5M3 10l6.75 4.5M21 10l-6.75 4.5m0 0l-1.14.76a2 2 0 01-2.22 0l-1.14-.76"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' mail
#' @name mail
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''mail'
#' @keywords rheroicons outline mail
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/mail.svg}
#' @examples
#' rheroicons::outline$mail(
#'   id = 'my_mail_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the mail icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$mail <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_mail", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' menu_alt_1
#' @name menu_alt_1
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''menu_alt_1'
#' @keywords rheroicons outline menu_alt_1
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/menu_alt_1.svg}
#' @examples
#' rheroicons::outline$menu_alt_1(
#'   id = 'my_menu_alt_1_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_1 icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$menu_alt_1 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_menu_alt_1", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 6h16M4 12h8m-8 6h16"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' menu_alt_2
#' @name menu_alt_2
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''menu_alt_2'
#' @keywords rheroicons outline menu_alt_2
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/menu_alt_2.svg}
#' @examples
#' rheroicons::outline$menu_alt_2(
#'   id = 'my_menu_alt_2_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_2 icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$menu_alt_2 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_menu_alt_2", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 6h16M4 12h16M4 18h7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' menu_alt_3
#' @name menu_alt_3
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''menu_alt_3'
#' @keywords rheroicons outline menu_alt_3
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/menu_alt_3.svg}
#' @examples
#' rheroicons::outline$menu_alt_3(
#'   id = 'my_menu_alt_3_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_3 icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$menu_alt_3 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_menu_alt_3", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 6h16M4 12h16m-7 6h7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' menu_alt_4
#' @name menu_alt_4
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''menu_alt_4'
#' @keywords rheroicons outline menu_alt_4
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/menu_alt_4.svg}
#' @examples
#' rheroicons::outline$menu_alt_4(
#'   id = 'my_menu_alt_4_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_4 icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$menu_alt_4 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_menu_alt_4", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 8h16M4 16h16"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' menu
#' @name menu
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''menu'
#' @keywords rheroicons outline menu
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/menu.svg}
#' @examples
#' rheroicons::outline$menu(
#'   id = 'my_menu_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$menu <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_menu", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 6h16M4 12h16M4 18h16"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' microphone
#' @name microphone
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''microphone'
#' @keywords rheroicons outline microphone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/microphone.svg}
#' @examples
#' rheroicons::outline$microphone(
#'   id = 'my_microphone_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the microphone icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$microphone <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_microphone", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M19 11a7 7 0 01-7 7m0 0a7 7 0 01-7-7m7 7v4m0 0H8m4 0h4m-4-8a3 3 0 01-3-3V5a3 3 0 116 0v6a3 3 0 01-3 3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' minus_circle
#' @name minus_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''minus_circle'
#' @keywords rheroicons outline minus_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/minus_circle.svg}
#' @examples
#' rheroicons::outline$minus_circle(
#'   id = 'my_minus_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the minus_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$minus_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_minus_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 12H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' moon
#' @name moon
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''moon'
#' @keywords rheroicons outline moon
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/moon.svg}
#' @examples
#' rheroicons::outline$moon(
#'   id = 'my_moon_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the moon icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$moon <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_moon", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' newspaper
#' @name newspaper
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''newspaper'
#' @keywords rheroicons outline newspaper
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/newspaper.svg}
#' @examples
#' rheroicons::outline$newspaper(
#'   id = 'my_newspaper_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the newspaper icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$newspaper <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_newspaper", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M19 20H5a2 2 0 01-2-2V6a2 2 0 012-2h10a2 2 0 012 2v1m2 13a2 2 0 01-2-2V7m2 13a2 2 0 002-2V9a2 2 0 00-2-2h-2m-4-3H9M7 16h6M7 8h6v4H7V8z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' office_building
#' @name office_building
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''office_building'
#' @keywords rheroicons outline office_building
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/office_building.svg}
#' @examples
#' rheroicons::outline$office_building(
#'   id = 'my_office_building_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the office_building icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$office_building <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_office_building", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M19 21V5a2 2 0 00-2-2H7a2 2 0 00-2 2v16m14 0h2m-2 0h-5m-9 0H3m2 0h5M9 7h1m-1 4h1m4-4h1m-1 4h1m-5 10v-5a1 1 0 011-1h2a1 1 0 011 1v5m-4 0h4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' paper_clip
#' @name paper_clip
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''paper_clip'
#' @keywords rheroicons outline paper_clip
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/paper_clip.svg}
#' @examples
#' rheroicons::outline$paper_clip(
#'   id = 'my_paper_clip_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the paper_clip icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$paper_clip <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_paper_clip", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15.172 7l-6.586 6.586a2 2 0 102.828 2.828l6.414-6.586a4 4 0 00-5.656-5.656l-6.415 6.585a6 6 0 108.486 8.486L20.5 13"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' pause
#' @name pause
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''pause'
#' @keywords rheroicons outline pause
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/pause.svg}
#' @examples
#' rheroicons::outline$pause(
#'   id = 'my_pause_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the pause icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$pause <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_pause", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M10 9v6m4-6v6m7-3a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' pencil_alt
#' @name pencil_alt
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''pencil_alt'
#' @keywords rheroicons outline pencil_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/pencil_alt.svg}
#' @examples
#' rheroicons::outline$pencil_alt(
#'   id = 'my_pencil_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the pencil_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$pencil_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_pencil_alt", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' pencil
#' @name pencil
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''pencil'
#' @keywords rheroicons outline pencil
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/pencil.svg}
#' @examples
#' rheroicons::outline$pencil(
#'   id = 'my_pencil_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the pencil icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$pencil <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_pencil", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' phone_incoming
#' @name phone_incoming
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''phone_incoming'
#' @keywords rheroicons outline phone_incoming
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/phone_incoming.svg}
#' @examples
#' rheroicons::outline$phone_incoming(
#'   id = 'my_phone_incoming_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the phone_incoming icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$phone_incoming <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_phone_incoming", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 3l-6 6m0 0V4m0 5h5M5 3a2 2 0 00-2 2v1c0 8.284 6.716 15 15 15h1a2 2 0 002-2v-3.28a1 1 0 00-.684-.948l-4.493-1.498a1 1 0 00-1.21.502l-1.13 2.257a11.042 11.042 0 01-5.516-5.517l2.257-1.128a1 1 0 00.502-1.21L9.228 3.683A1 1 0 008.279 3H5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' phone_outgoing
#' @name phone_outgoing
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''phone_outgoing'
#' @keywords rheroicons outline phone_outgoing
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/phone_outgoing.svg}
#' @examples
#' rheroicons::outline$phone_outgoing(
#'   id = 'my_phone_outgoing_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the phone_outgoing icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$phone_outgoing <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_phone_outgoing", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 3h5m0 0v5m0-5l-6 6M5 3a2 2 0 00-2 2v1c0 8.284 6.716 15 15 15h1a2 2 0 002-2v-3.28a1 1 0 00-.684-.948l-4.493-1.498a1 1 0 00-1.21.502l-1.13 2.257a11.042 11.042 0 01-5.516-5.517l2.257-1.128a1 1 0 00.502-1.21L9.228 3.683A1 1 0 008.279 3H5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' phone
#' @name phone
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''phone'
#' @keywords rheroicons outline phone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/phone.svg}
#' @examples
#' rheroicons::outline$phone(
#'   id = 'my_phone_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the phone icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$phone <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_phone", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 5a2 2 0 012-2h3.28a1 1 0 01.948.684l1.498 4.493a1 1 0 01-.502 1.21l-2.257 1.13a11.042 11.042 0 005.516 5.516l1.13-2.257a1 1 0 011.21-.502l4.493 1.498a1 1 0 01.684.949V19a2 2 0 01-2 2h-1C9.716 21 3 14.284 3 6V5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' photograph
#' @name photograph
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''photograph'
#' @keywords rheroicons outline photograph
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/photograph.svg}
#' @examples
#' rheroicons::outline$photograph(
#'   id = 'my_photograph_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the photograph icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$photograph <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_photograph", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' play
#' @name play
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''play'
#' @keywords rheroicons outline play
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/play.svg}
#' @examples
#' rheroicons::outline$play(
#'   id = 'my_play_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the play icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$play <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_play", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M14.752 11.168l-3.197-2.132A1 1 0 0010 9.87v4.263a1 1 0 001.555.832l3.197-2.132a1 1 0 000-1.664z")), 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' plus_circle
#' @name plus_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''plus_circle'
#' @keywords rheroicons outline plus_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/plus_circle.svg}
#' @examples
#' rheroicons::outline$plus_circle(
#'   id = 'my_plus_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the plus_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$plus_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_plus_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 9v3m0 0v3m0-3h3m-3 0H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' plus
#' @name plus
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''plus'
#' @keywords rheroicons outline plus
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/plus.svg}
#' @examples
#' rheroicons::outline$plus(
#'   id = 'my_plus_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the plus icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$plus <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_plus", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 4v16m8-8H4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' printer
#' @name printer
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''printer'
#' @keywords rheroicons outline printer
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/printer.svg}
#' @examples
#' rheroicons::outline$printer(
#'   id = 'my_printer_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the printer icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$printer <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_printer", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17 17h2a2 2 0 002-2v-4a2 2 0 00-2-2H5a2 2 0 00-2 2v4a2 2 0 002 2h2m2 4h6a2 2 0 002-2v-4a2 2 0 00-2-2H9a2 2 0 00-2 2v4a2 2 0 002 2zm8-12V5a2 2 0 00-2-2H9a2 2 0 00-2 2v4h10z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' puzzle
#' @name puzzle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''puzzle'
#' @keywords rheroicons outline puzzle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/puzzle.svg}
#' @examples
#' rheroicons::outline$puzzle(
#'   id = 'my_puzzle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the puzzle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$puzzle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_puzzle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M11 4a2 2 0 114 0v1a1 1 0 001 1h3a1 1 0 011 1v3a1 1 0 01-1 1h-1a2 2 0 100 4h1a1 1 0 011 1v3a1 1 0 01-1 1h-3a1 1 0 01-1-1v-1a2 2 0 10-4 0v1a1 1 0 01-1 1H7a1 1 0 01-1-1v-3a1 1 0 00-1-1H4a2 2 0 110-4h1a1 1 0 001-1V7a1 1 0 011-1h3a1 1 0 001-1V4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' qrcode
#' @name qrcode
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''qrcode'
#' @keywords rheroicons outline qrcode
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/qrcode.svg}
#' @examples
#' rheroicons::outline$qrcode(
#'   id = 'my_qrcode_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the qrcode icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$qrcode <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_qrcode", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 4v1m6 11h2m-6 0h-2v4m0-11v3m0 0h.01M12 12h4.01M16 20h4M4 12h4m12 0h.01M5 8h2a1 1 0 001-1V5a1 1 0 00-1-1H5a1 1 0 00-1 1v2a1 1 0 001 1zm12 0h2a1 1 0 001-1V5a1 1 0 00-1-1h-2a1 1 0 00-1 1v2a1 1 0 001 1zM5 20h2a1 1 0 001-1v-2a1 1 0 00-1-1H5a1 1 0 00-1 1v2a1 1 0 001 1z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' question_mark_circle
#' @name question_mark_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''question_mark_circle'
#' @keywords rheroicons outline question_mark_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/question_mark_circle.svg}
#' @examples
#' rheroicons::outline$question_mark_circle(
#'   id = 'my_question_mark_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the question_mark_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$question_mark_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_question_mark_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' receipt_refund
#' @name receipt_refund
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''receipt_refund'
#' @keywords rheroicons outline receipt_refund
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/receipt_refund.svg}
#' @examples
#' rheroicons::outline$receipt_refund(
#'   id = 'my_receipt_refund_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the receipt_refund icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$receipt_refund <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_receipt_refund", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 15v-1a4 4 0 00-4-4H8m0 0l3 3m-3-3l3-3m9 14V5a2 2 0 00-2-2H6a2 2 0 00-2 2v16l4-2 4 2 4-2 4 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' refresh
#' @name refresh
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''refresh'
#' @keywords rheroicons outline refresh
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/refresh.svg}
#' @examples
#' rheroicons::outline$refresh(
#'   id = 'my_refresh_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the refresh icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$refresh <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_refresh", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' reply
#' @name reply
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''reply'
#' @keywords rheroicons outline reply
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/reply.svg}
#' @examples
#' rheroicons::outline$reply(
#'   id = 'my_reply_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the reply icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$reply <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_reply", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 10h10a8 8 0 018 8v2M3 10l6 6m-6-6l6-6"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' scale
#' @name scale
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''scale'
#' @keywords rheroicons outline scale
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/scale.svg}
#' @examples
#' rheroicons::outline$scale(
#'   id = 'my_scale_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the scale icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$scale <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_scale", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 6l3 1m0 0l-3 9a5.002 5.002 0 006.001 0M6 7l3 9M6 7l6-2m6 2l3-1m-3 1l-3 9a5.002 5.002 0 006.001 0M18 7l3 9m-3-9l-6-2m0-2v2m0 16V5m0 16H9m3 0h3"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' search
#' @name search
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''search'
#' @keywords rheroicons outline search
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/search.svg}
#' @examples
#' rheroicons::outline$search(
#'   id = 'my_search_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the search icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$search <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_search", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' selector
#' @name selector
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''selector'
#' @keywords rheroicons outline selector
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/selector.svg}
#' @examples
#' rheroicons::outline$selector(
#'   id = 'my_selector_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the selector icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$selector <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_selector", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 9l4-4 4 4m0 6l-4 4-4-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' share
#' @name share
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''share'
#' @keywords rheroicons outline share
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/share.svg}
#' @examples
#' rheroicons::outline$share(
#'   id = 'my_share_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the share icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$share <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_share", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' shield_check
#' @name shield_check
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''shield_check'
#' @keywords rheroicons outline shield_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/shield_check.svg}
#' @examples
#' rheroicons::outline$shield_check(
#'   id = 'my_shield_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shield_check icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$shield_check <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_shield_check", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' shield_exclamation
#' @name shield_exclamation
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''shield_exclamation'
#' @keywords rheroicons outline shield_exclamation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/shield_exclamation.svg}
#' @examples
#' rheroicons::outline$shield_exclamation(
#'   id = 'my_shield_exclamation_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shield_exclamation icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$shield_exclamation <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_shield_exclamation", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M20.618 5.984A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016zM12 9v2m0 4h.01"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' shopping_bag
#' @name shopping_bag
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''shopping_bag'
#' @keywords rheroicons outline shopping_bag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/shopping_bag.svg}
#' @examples
#' rheroicons::outline$shopping_bag(
#'   id = 'my_shopping_bag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shopping_bag icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$shopping_bag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_shopping_bag", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 11V7a4 4 0 00-8 0v4M5 9h14l1 12H4L5 9z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' shopping_cart
#' @name shopping_cart
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''shopping_cart'
#' @keywords rheroicons outline shopping_cart
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/shopping_cart.svg}
#' @examples
#' rheroicons::outline$shopping_cart(
#'   id = 'my_shopping_cart_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shopping_cart icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$shopping_cart <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_shopping_cart", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 3h2l.4 2M7 13h10l4-8H5.4M7 13L5.4 5M7 13l-2.293 2.293c-.63.63-.184 1.707.707 1.707H17m0 0a2 2 0 100 4 2 2 0 000-4zm-8 2a2 2 0 11-4 0 2 2 0 014 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' sort_ascending
#' @name sort_ascending
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''sort_ascending'
#' @keywords rheroicons outline sort_ascending
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/sort_ascending.svg}
#' @examples
#' rheroicons::outline$sort_ascending(
#'   id = 'my_sort_ascending_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sort_ascending icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$sort_ascending <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_sort_ascending", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 4h13M3 8h9m-9 4h6m4 0l4-4m0 0l4 4m-4-4v12"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' sort_descending
#' @name sort_descending
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''sort_descending'
#' @keywords rheroicons outline sort_descending
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/sort_descending.svg}
#' @examples
#' rheroicons::outline$sort_descending(
#'   id = 'my_sort_descending_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sort_descending icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$sort_descending <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_sort_descending", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 4h13M3 8h9m-9 4h9m5-4v12m0 0l-4-4m4 4l4-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' sparkles
#' @name sparkles
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''sparkles'
#' @keywords rheroicons outline sparkles
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/sparkles.svg}
#' @examples
#' rheroicons::outline$sparkles(
#'   id = 'my_sparkles_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sparkles icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$sparkles <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_sparkles", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 3v4M3 5h4M6 17v4m-2-2h4m5-16l2.286 6.857L21 12l-5.714 2.143L13 21l-2.286-6.857L5 12l5.714-2.143L13 3z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' speakerphone
#' @name speakerphone
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''speakerphone'
#' @keywords rheroicons outline speakerphone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/speakerphone.svg}
#' @examples
#' rheroicons::outline$speakerphone(
#'   id = 'my_speakerphone_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the speakerphone icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$speakerphone <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_speakerphone", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M11 5.882V19.24a1.76 1.76 0 01-3.417.592l-2.147-6.15M18 13a3 3 0 100-6M5.436 13.683A4.001 4.001 0 017 6h1.832c4.1 0 7.625-1.234 9.168-3v14c-1.543-1.766-5.067-3-9.168-3H7a3.988 3.988 0 01-1.564-.317z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' star
#' @name star
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''star'
#' @keywords rheroicons outline star
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/star.svg}
#' @examples
#' rheroicons::outline$star(
#'   id = 'my_star_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the star icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$star <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_star", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M11.049 2.927c.3-.921 1.603-.921 1.902 0l1.519 4.674a1 1 0 00.95.69h4.915c.969 0 1.371 1.24.588 1.81l-3.976 2.888a1 1 0 00-.363 1.118l1.518 4.674c.3.922-.755 1.688-1.538 1.118l-3.976-2.888a1 1 0 00-1.176 0l-3.976 2.888c-.783.57-1.838-.197-1.538-1.118l1.518-4.674a1 1 0 00-.363-1.118l-3.976-2.888c-.784-.57-.38-1.81.588-1.81h4.914a1 1 0 00.951-.69l1.519-4.674z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' stop
#' @name stop
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''stop'
#' @keywords rheroicons outline stop
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/stop.svg}
#' @examples
#' rheroicons::outline$stop(
#'   id = 'my_stop_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the stop icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$stop <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_stop", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 12a9 9 0 11-18 0 9 9 0 0118 0z")), tag(`_tag_name` = "path", 
            list(`stroke-linecap` = "round", `stroke-linejoin` = "round", `stroke-width` = "2", 
                d = "M9 10a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1h-4a1 1 0 01-1-1v-4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' sun
#' @name sun
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''sun'
#' @keywords rheroicons outline sun
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/sun.svg}
#' @examples
#' rheroicons::outline$sun(
#'   id = 'my_sun_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sun icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$sun <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_sun", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' support
#' @name support
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''support'
#' @keywords rheroicons outline support
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/support.svg}
#' @examples
#' rheroicons::outline$support(
#'   id = 'my_support_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the support icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$support <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_support", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M18.364 5.636l-3.536 3.536m0 5.656l3.536 3.536M9.172 9.172L5.636 5.636m3.536 9.192l-3.536 3.536M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-5 0a4 4 0 11-8 0 4 4 0 018 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' switch_horizontal
#' @name switch_horizontal
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''switch_horizontal'
#' @keywords rheroicons outline switch_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/switch_horizontal.svg}
#' @examples
#' rheroicons::outline$switch_horizontal(
#'   id = 'my_switch_horizontal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the switch_horizontal icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$switch_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_switch_horizontal", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 7h12m0 0l-4-4m4 4l-4 4m0 6H4m0 0l4 4m-4-4l4-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' switch_vertical
#' @name switch_vertical
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''switch_vertical'
#' @keywords rheroicons outline switch_vertical
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/switch_vertical.svg}
#' @examples
#' rheroicons::outline$switch_vertical(
#'   id = 'my_switch_vertical_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the switch_vertical icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$switch_vertical <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_switch_vertical", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 16V4m0 0L3 8m4-4l4 4m6 0v12m0 0l4-4m-4 4l-4-4"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' tag
#' @name tag
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''tag'
#' @keywords rheroicons outline tag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/tag.svg}
#' @examples
#' rheroicons::outline$tag(
#'   id = 'my_tag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the tag icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$tag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_tag", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M7 7h.01M7 3h5c.512 0 1.024.195 1.414.586l7 7a2 2 0 010 2.828l-7 7a2 2 0 01-2.828 0l-7-7A1.994 1.994 0 013 12V7a4 4 0 014-4z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' template
#' @name template
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''template'
#' @keywords rheroicons outline template
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/template.svg}
#' @examples
#' rheroicons::outline$template(
#'   id = 'my_template_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the template icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$template <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_template", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 5a1 1 0 011-1h14a1 1 0 011 1v2a1 1 0 01-1 1H5a1 1 0 01-1-1V5zM4 13a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H5a1 1 0 01-1-1v-6zM16 13a1 1 0 011-1h2a1 1 0 011 1v6a1 1 0 01-1 1h-2a1 1 0 01-1-1v-6z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' terminal
#' @name terminal
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''terminal'
#' @keywords rheroicons outline terminal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/terminal.svg}
#' @examples
#' rheroicons::outline$terminal(
#'   id = 'my_terminal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the terminal icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$terminal <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_terminal", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M8 9l3 3-3 3m5 0h3M5 20h14a2 2 0 002-2V6a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' thumb_down
#' @name thumb_down
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''thumb_down'
#' @keywords rheroicons outline thumb_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/thumb_down.svg}
#' @examples
#' rheroicons::outline$thumb_down(
#'   id = 'my_thumb_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the thumb_down icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$thumb_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_thumb_down", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M10 14H5.236a2 2 0 01-1.789-2.894l3.5-7A2 2 0 018.736 3h4.018a2 2 0 01.485.06l3.76.94m-7 10v5a2 2 0 002 2h.096c.5 0 .905-.405.905-.904 0-.715.211-1.413.608-2.008L17 13V4m-7 10h2m5-10h2a2 2 0 012 2v6a2 2 0 01-2 2h-2.5"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' thumb_up
#' @name thumb_up
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''thumb_up'
#' @keywords rheroicons outline thumb_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/thumb_up.svg}
#' @examples
#' rheroicons::outline$thumb_up(
#'   id = 'my_thumb_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the thumb_up icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$thumb_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_thumb_up", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M14 10h4.764a2 2 0 011.789 2.894l-3.5 7A2 2 0 0115.263 21h-4.017c-.163 0-.326-.02-.485-.06L7 20m7-10V5a2 2 0 00-2-2h-.095c-.5 0-.905.405-.905.905 0 .714-.211 1.412-.608 2.006L7 11v9m7-10h-2M7 20H5a2 2 0 01-2-2v-6a2 2 0 012-2h2.5"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' ticket
#' @name ticket
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''ticket'
#' @keywords rheroicons outline ticket
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/ticket.svg}
#' @examples
#' rheroicons::outline$ticket(
#'   id = 'my_ticket_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the ticket icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$ticket <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_ticket", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15 5v2m0 4v2m0 4v2M5 5a2 2 0 00-2 2v3a2 2 0 110 4v3a2 2 0 002 2h14a2 2 0 002-2v-3a2 2 0 110-4V7a2 2 0 00-2-2H5z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' translate
#' @name translate
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''translate'
#' @keywords rheroicons outline translate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/translate.svg}
#' @examples
#' rheroicons::outline$translate(
#'   id = 'my_translate_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the translate icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$translate <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_translate", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M3 5h12M9 3v2m1.048 9.5A18.022 18.022 0 016.412 9m6.088 9h7M11 21l5-10 5 10M12.751 5C11.783 10.77 8.07 15.61 3 18.129"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' trash
#' @name trash
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''trash'
#' @keywords rheroicons outline trash
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/trash.svg}
#' @examples
#' rheroicons::outline$trash(
#'   id = 'my_trash_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the trash icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$trash <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_trash", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' trending_down
#' @name trending_down
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''trending_down'
#' @keywords rheroicons outline trending_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/trending_down.svg}
#' @examples
#' rheroicons::outline$trending_down(
#'   id = 'my_trending_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the trending_down icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$trending_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_trending_down", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13 17h8m0 0V9m0 8l-8-8-4 4-6-6"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' trending_up
#' @name trending_up
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''trending_up'
#' @keywords rheroicons outline trending_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/trending_up.svg}
#' @examples
#' rheroicons::outline$trending_up(
#'   id = 'my_trending_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the trending_up icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$trending_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_trending_up", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13 7h8m0 0v8m0-8l-8 8-4-4-6 6"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' upload
#' @name upload
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''upload'
#' @keywords rheroicons outline upload
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/upload.svg}
#' @examples
#' rheroicons::outline$upload(
#'   id = 'my_upload_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the upload icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$upload <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_upload", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' user_add
#' @name user_add
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''user_add'
#' @keywords rheroicons outline user_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/user_add.svg}
#' @examples
#' rheroicons::outline$user_add(
#'   id = 'my_user_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_add icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$user_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_user_add", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M18 9v3m0 0v3m0-3h3m-3 0h-3m-2-5a4 4 0 11-8 0 4 4 0 018 0zM3 20a6 6 0 0112 0v1H3v-1z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' user_circle
#' @name user_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''user_circle'
#' @keywords rheroicons outline user_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/user_circle.svg}
#' @examples
#' rheroicons::outline$user_circle(
#'   id = 'my_user_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$user_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_user_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5.121 17.804A13.937 13.937 0 0112 16c2.5 0 4.847.655 6.879 1.804M15 10a3 3 0 11-6 0 3 3 0 016 0zm6 2a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' user_group
#' @name user_group
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''user_group'
#' @keywords rheroicons outline user_group
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/user_group.svg}
#' @examples
#' rheroicons::outline$user_group(
#'   id = 'my_user_group_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_group icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$user_group <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_user_group", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' user_remove
#' @name user_remove
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''user_remove'
#' @keywords rheroicons outline user_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/user_remove.svg}
#' @examples
#' rheroicons::outline$user_remove(
#'   id = 'my_user_remove_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_remove icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$user_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_user_remove", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M13 7a4 4 0 11-8 0 4 4 0 018 0zM9 14a6 6 0 00-6 6v1h12v-1a6 6 0 00-6-6zM21 12h-6"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' user
#' @name user
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''user'
#' @keywords rheroicons outline user
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/user.svg}
#' @examples
#' rheroicons::outline$user(
#'   id = 'my_user_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$user <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_user", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' users
#' @name users
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''users'
#' @keywords rheroicons outline users
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/users.svg}
#' @examples
#' rheroicons::outline$users(
#'   id = 'my_users_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the users icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$users <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_users", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' view_boards
#' @name view_boards
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''view_boards'
#' @keywords rheroicons outline view_boards
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/view_boards.svg}
#' @examples
#' rheroicons::outline$view_boards(
#'   id = 'my_view_boards_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_boards icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$view_boards <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_view_boards", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M9 17V7m0 10a2 2 0 01-2 2H5a2 2 0 01-2-2V7a2 2 0 012-2h2a2 2 0 012 2m0 10a2 2 0 002 2h2a2 2 0 002-2M9 7a2 2 0 012-2h2a2 2 0 012 2m0 10V7m0 10a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 00-2-2h-2a2 2 0 00-2 2"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' view_grid_add
#' @name view_grid_add
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''view_grid_add'
#' @keywords rheroicons outline view_grid_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/view_grid_add.svg}
#' @examples
#' rheroicons::outline$view_grid_add(
#'   id = 'my_view_grid_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_grid_add icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$view_grid_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_view_grid_add", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M17 14v6m-3-3h6M6 10h2a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v2a2 2 0 002 2zm10 0h2a2 2 0 002-2V6a2 2 0 00-2-2h-2a2 2 0 00-2 2v2a2 2 0 002 2zM6 20h2a2 2 0 002-2v-2a2 2 0 00-2-2H6a2 2 0 00-2 2v2a2 2 0 002 2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' view_grid
#' @name view_grid
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''view_grid'
#' @keywords rheroicons outline view_grid
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/view_grid.svg}
#' @examples
#' rheroicons::outline$view_grid(
#'   id = 'my_view_grid_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_grid icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$view_grid <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_view_grid", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2V6zM14 6a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V6zM4 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2H6a2 2 0 01-2-2v-2zM14 16a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' view_list
#' @name view_list
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''view_list'
#' @keywords rheroicons outline view_list
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/view_list.svg}
#' @examples
#' rheroicons::outline$view_list(
#'   id = 'my_view_list_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_list icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$view_list <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_view_list", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M4 6h16M4 10h16M4 14h16M4 18h16"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' volume_off
#' @name volume_off
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''volume_off'
#' @keywords rheroicons outline volume_off
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/volume_off.svg}
#' @examples
#' rheroicons::outline$volume_off(
#'   id = 'my_volume_off_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the volume_off icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$volume_off <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_volume_off", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5.586 15H4a1 1 0 01-1-1v-4a1 1 0 011-1h1.586l4.707-4.707C10.923 3.663 12 4.109 12 5v14c0 .891-1.077 1.337-1.707.707L5.586 15z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(`stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M17 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' volume_up
#' @name volume_up
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''volume_up'
#' @keywords rheroicons outline volume_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/volume_up.svg}
#' @examples
#' rheroicons::outline$volume_up(
#'   id = 'my_volume_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the volume_up icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$volume_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_volume_up", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M15.536 8.464a5 5 0 010 7.072m2.828-9.9a9 9 0 010 12.728M5.586 15H4a1 1 0 01-1-1v-4a1 1 0 011-1h1.586l4.707-4.707C10.923 3.663 12 4.109 12 5v14c0 .891-1.077 1.337-1.707.707L5.586 15z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' x_circle
#' @name x_circle
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''x_circle'
#' @keywords rheroicons outline x_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/x_circle.svg}
#' @examples
#' rheroicons::outline$x_circle(
#'   id = 'my_x_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the x_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$x_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_x_circle", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' x
#' @name x
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''x'
#' @keywords rheroicons outline x
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/x.svg}
#' @examples
#' rheroicons::outline$x(
#'   id = 'my_x_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the x icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$x <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_x", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M6 18L18 6M6 6l12 12"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' zoom_in
#' @name zoom_in
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''zoom_in'
#' @keywords rheroicons outline zoom_in
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/zoom_in.svg}
#' @examples
#' rheroicons::outline$zoom_in(
#'   id = 'my_zoom_in_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the zoom_in icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$zoom_in <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_zoom_in", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0zM10 7v3m0 0v3m0-3h3m-3 0H7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

#' zoom_out
#' @name zoom_out
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''zoom_out'
#' @keywords rheroicons outline zoom_out
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/outline/zoom_out.svg}
#' @examples
#' rheroicons::outline$zoom_out(
#'   id = 'my_zoom_out_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the zoom_out icon'
#' )
#' @importFrom htmltools tag
#' @export
outline$zoom_out <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_outline rheroicons_zoom_out", 
        width = "24", height = "24", fill = "none", viewBox = "0 0 24 24", stroke = "currentColor", 
        tag(`_tag_name` = "path", list(`stroke-linecap` = "round", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0zM13 10H7"))))
    if (!is.null(id)) {
        svg$attribs$id <- id
    }
    if (!is.null(class)) {
        svg$attribs$class <- paste0(svg$attribs$class, " ", class)
    }
    if (isTRUE(aria_hidden)) {
        svg$attribs$`aria-hidden` <- "true"
    }
    if (!is.null(title)) {
        stopifnot(is.character(title))
        svg$children <- tagList(tag("title", list(title)), svg$children)
    }
    return(svg)
}

