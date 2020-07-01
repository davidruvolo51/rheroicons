#' SOLID SVG Icons
#' @name solid
#' @keywords rheroicons solid
#' @return solid heroicons
#' @references
#' \url{https://github.com/refactoringui/heroicons}
#' \url{https://davidruvolo.shinyapps.io/rheroicons-demo/}
#' @examples
#' rheroicons::solid$book_open()
#' rheroicons::solid$book_open(id = 'myBookIcon')
#' rheroicons::solid$book_open(class = 'my-icon-set')
#' rheroicons::solid$book_open(aria_hidden = FALSE, title = 'read document')
#' @importFrom htmltools tag
#' @export
solid <- list()

#' adjustments
#' @name adjustments
#' @param id a unique ID to be applied to the svg icon
#' @param class a css class to be applied to the svg icon
#' @param aria_hidden should the icon be readable by screen readers (default: false)
#' @param title a string that describes the icon(should be used if aria_hidden is FALSE)
#' @return Returns the svg markup for the heroicon ''adjustments'
#' @keywords rheroicons solid adjustments
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/adjustments.svg}
#' @examples
#' rheroicons::solid$adjustments(
#'   id = 'my_adjustments_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the adjustments icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$adjustments <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_adjustments", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M5 4a1 1 0 00-2 0v7.268a2 2 0 000 3.464V16a1 1 0 102 0v-1.268a2 2 0 000-3.464V4zM11 4a1 1 0 10-2 0v1.268a2 2 0 000 3.464V16a1 1 0 102 0V8.732a2 2 0 000-3.464V4zM16 3a1 1 0 011 1v7.268a2 2 0 010 3.464V16a1 1 0 11-2 0v-1.268a2 2 0 010-3.464V4a1 1 0 011-1z"))))
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
#' @keywords rheroicons solid annotation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/annotation.svg}
#' @examples
#' rheroicons::solid$annotation(
#'   id = 'my_annotation_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the annotation icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$annotation <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_annotation", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 13V5a2 2 0 00-2-2H4a2 2 0 00-2 2v8a2 2 0 002 2h3l3 3 3-3h3a2 2 0 002-2zM5 7a1 1 0 011-1h8a1 1 0 110 2H6a1 1 0 01-1-1zm1 3a1 1 0 100 2h3a1 1 0 100-2H6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid archive
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/archive.svg}
#' @examples
#' rheroicons::solid$archive(
#'   id = 'my_archive_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the archive icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$archive <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_archive", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M4 3a2 2 0 100 4h12a2 2 0 100-4H4z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 8h14v7a2 2 0 01-2 2H5a2 2 0 01-2-2V8zm5 3a1 1 0 011-1h2a1 1 0 110 2H9a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_circle_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_down.svg}
#' @examples
#' rheroicons::solid$arrow_circle_down(
#'   id = 'my_arrow_circle_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_down icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_circle_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_circle_down", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-11a1 1 0 10-2 0v3.586L7.707 9.293a1 1 0 00-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L11 10.586V7z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_circle_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_left.svg}
#' @examples
#' rheroicons::solid$arrow_circle_left(
#'   id = 'my_arrow_circle_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_left icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_circle_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_circle_left", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm.707-10.293a1 1 0 00-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L9.414 11H13a1 1 0 100-2H9.414l1.293-1.293z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_circle_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_right.svg}
#' @examples
#' rheroicons::solid$arrow_circle_right(
#'   id = 'my_arrow_circle_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_right icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_circle_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_circle_right", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-8.707l-3-3a1 1 0 00-1.414 1.414L10.586 9H7a1 1 0 100 2h3.586l-1.293 1.293a1 1 0 101.414 1.414l3-3a1 1 0 000-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_circle_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_circle_up.svg}
#' @examples
#' rheroicons::solid$arrow_circle_up(
#'   id = 'my_arrow_circle_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_circle_up icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_circle_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_circle_up", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-8.707l-3-3a1 1 0 00-1.414 0l-3 3a1 1 0 001.414 1.414L9 9.414V13a1 1 0 102 0V9.414l1.293 1.293a1 1 0 001.414-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_down.svg}
#' @examples
#' rheroicons::solid$arrow_down(
#'   id = 'my_arrow_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_down icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_down", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M16.707 10.293a1 1 0 010 1.414l-6 6a1 1 0 01-1.414 0l-6-6a1 1 0 111.414-1.414L9 14.586V3a1 1 0 012 0v11.586l4.293-4.293a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_left.svg}
#' @examples
#' rheroicons::solid$arrow_left(
#'   id = 'my_arrow_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_left icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_left", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M9.707 16.707a1 1 0 01-1.414 0l-6-6a1 1 0 010-1.414l6-6a1 1 0 011.414 1.414L5.414 9H17a1 1 0 110 2H5.414l4.293 4.293a1 1 0 010 1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_narrow_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_down.svg}
#' @examples
#' rheroicons::solid$arrow_narrow_down(
#'   id = 'my_arrow_narrow_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_down icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_narrow_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_narrow_down", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M14.707 12.293a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 111.414-1.414L9 14.586V3a1 1 0 012 0v11.586l2.293-2.293a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_narrow_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_left.svg}
#' @examples
#' rheroicons::solid$arrow_narrow_left(
#'   id = 'my_arrow_narrow_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_left icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_narrow_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_narrow_left", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M7.707 14.707a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 1.414L5.414 9H17a1 1 0 110 2H5.414l2.293 2.293a1 1 0 010 1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_narrow_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_right.svg}
#' @examples
#' rheroicons::solid$arrow_narrow_right(
#'   id = 'my_arrow_narrow_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_right icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_narrow_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_narrow_right", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M12.293 5.293a1 1 0 011.414 0l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414-1.414L14.586 11H3a1 1 0 110-2h11.586l-2.293-2.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_narrow_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_narrow_up.svg}
#' @examples
#' rheroicons::solid$arrow_narrow_up(
#'   id = 'my_arrow_narrow_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_narrow_up icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_narrow_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_narrow_up", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5.293 7.707a1 1 0 010-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 01-1.414 1.414L11 5.414V17a1 1 0 11-2 0V5.414L6.707 7.707a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_right.svg}
#' @examples
#' rheroicons::solid$arrow_right(
#'   id = 'my_arrow_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_right icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_right", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10.293 3.293a1 1 0 011.414 0l6 6a1 1 0 010 1.414l-6 6a1 1 0 01-1.414-1.414L14.586 11H3a1 1 0 110-2h11.586l-4.293-4.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrow_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrow_up.svg}
#' @examples
#' rheroicons::solid$arrow_up(
#'   id = 'my_arrow_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrow_up icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrow_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrow_up", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3.293 9.707a1 1 0 010-1.414l6-6a1 1 0 011.414 0l6 6a1 1 0 01-1.414 1.414L11 5.414V17a1 1 0 11-2 0V5.414L4.707 9.707a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid arrows_expand
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/arrows_expand.svg}
#' @examples
#' rheroicons::solid$arrows_expand(
#'   id = 'my_arrows_expand_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the arrows_expand icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$arrows_expand <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_arrows_expand", 
        width = "19", height = "20", viewBox = "0 0 19 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(stroke = "#374151", `stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M3 8V4m0 0h4M3 4l4 4m8 0V4m0 0h-4m4 0l-4 4m-8 4v4m0 0h4m-4 0l4-4m8 4l-4-4m4 4v-4m0 4h-4"))))
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
#' @keywords rheroicons solid at_symbol
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/at_symbol.svg}
#' @examples
#' rheroicons::solid$at_symbol(
#'   id = 'my_at_symbol_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the at_symbol icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$at_symbol <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_at_symbol", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M14.243 5.757a6 6 0 10-.986 9.284 1 1 0 111.087 1.678A8 8 0 1118 10a3 3 0 01-4.8 2.401A4 4 0 1114 10a1 1 0 102 0c0-1.537-.586-3.07-1.757-4.243zM12 10a2 2 0 10-4 0 2 2 0 004 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid badge_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/badge_check.svg}
#' @examples
#' rheroicons::solid$badge_check(
#'   id = 'my_badge_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the badge_check icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$badge_check <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_badge_check", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6.267 3.455a3.066 3.066 0 001.745-.723 3.066 3.066 0 013.976 0 3.066 3.066 0 001.745.723 3.066 3.066 0 012.812 2.812c.051.643.304 1.254.723 1.745a3.066 3.066 0 010 3.976 3.066 3.066 0 00-.723 1.745 3.066 3.066 0 01-2.812 2.812 3.066 3.066 0 00-1.745.723 3.066 3.066 0 01-3.976 0 3.066 3.066 0 00-1.745-.723 3.066 3.066 0 01-2.812-2.812 3.066 3.066 0 00-.723-1.745 3.066 3.066 0 010-3.976 3.066 3.066 0 00.723-1.745 3.066 3.066 0 012.812-2.812zm7.44 5.252a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid ban
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/ban.svg}
#' @examples
#' rheroicons::solid$ban(
#'   id = 'my_ban_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the ban icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$ban <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_ban", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M13.477 14.89A6 6 0 015.11 6.524l8.367 8.368zm1.414-1.414L6.524 5.11a6 6 0 018.367 8.367zM18 10a8 8 0 11-16 0 8 8 0 0116 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid bell
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/bell.svg}
#' @examples
#' rheroicons::solid$bell(
#'   id = 'my_bell_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the bell icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$bell <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_bell", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M10 2a6 6 0 00-6 6v3.586l-.707.707A1 1 0 004 14h12a1 1 0 00.707-1.707L16 11.586V8a6 6 0 00-6-6zM10 18a3 3 0 01-3-3h6a3 3 0 01-3 3z"))))
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
#' @keywords rheroicons solid book_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/book_open.svg}
#' @examples
#' rheroicons::solid$book_open(
#'   id = 'my_book_open_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the book_open icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$book_open <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_book_open", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M9 4.804A7.968 7.968 0 005.5 4c-1.255 0-2.443.29-3.5.804v10A7.969 7.969 0 015.5 14c1.669 0 3.218.51 4.5 1.385A7.962 7.962 0 0114.5 14c1.255 0 2.443.29 3.5.804v-10A7.968 7.968 0 0014.5 4c-1.255 0-2.443.29-3.5.804V12a1 1 0 11-2 0V4.804z"))))
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
#' @keywords rheroicons solid bookmark_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/bookmark_alt.svg}
#' @examples
#' rheroicons::solid$bookmark_alt(
#'   id = 'my_bookmark_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the bookmark_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$bookmark_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_bookmark_alt", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 5a2 2 0 012-2h10a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5zm11 1H6v8l4-2 4 2V6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid bookmark
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/bookmark.svg}
#' @examples
#' rheroicons::solid$bookmark(
#'   id = 'my_bookmark_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the bookmark icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$bookmark <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_bookmark", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M5 4a2 2 0 012-2h6a2 2 0 012 2v14l-5-2.5L5 18V4z"))))
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
#' @keywords rheroicons solid briefcase
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/briefcase.svg}
#' @examples
#' rheroicons::solid$briefcase(
#'   id = 'my_briefcase_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the briefcase icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$briefcase <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_briefcase", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6 6V5a3 3 0 013-3h2a3 3 0 013 3v1h2a2 2 0 012 2v3.57A22.952 22.952 0 0110 13a22.95 22.95 0 01-8-1.43V8a2 2 0 012-2h2zm2-1a1 1 0 011-1h2a1 1 0 011 1v1H8V5zm1 5a1 1 0 011-1h.01a1 1 0 110 2H10a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M2 13.692V16a2 2 0 002 2h12a2 2 0 002-2v-2.308A24.974 24.974 0 0110 15c-2.796 0-5.487-.46-8-1.308z"))))
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
#' @keywords rheroicons solid calendar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/calendar.svg}
#' @examples
#' rheroicons::solid$calendar(
#'   id = 'my_calendar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the calendar icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$calendar <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_calendar", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6 2a1 1 0 00-1 1v1H4a2 2 0 00-2 2v10a2 2 0 002 2h12a2 2 0 002-2V6a2 2 0 00-2-2h-1V3a1 1 0 10-2 0v1H7V3a1 1 0 00-1-1zm0 5a1 1 0 000 2h8a1 1 0 100-2H6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid camera
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/camera.svg}
#' @examples
#' rheroicons::solid$camera(
#'   id = 'my_camera_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the camera icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$camera <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_camera", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 5a2 2 0 00-2 2v8a2 2 0 002 2h12a2 2 0 002-2V7a2 2 0 00-2-2h-1.586a1 1 0 01-.707-.293l-1.121-1.121A2 2 0 0011.172 3H8.828a2 2 0 00-1.414.586L6.293 4.707A1 1 0 015.586 5H4zm6 9a3 3 0 100-6 3 3 0 000 6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid cash
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cash.svg}
#' @examples
#' rheroicons::solid$cash(
#'   id = 'my_cash_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cash icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$cash <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_cash", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 4a2 2 0 00-2 2v4a2 2 0 002 2V6h10a2 2 0 00-2-2H4zm2 6a2 2 0 012-2h8a2 2 0 012 2v4a2 2 0 01-2 2H8a2 2 0 01-2-2v-4zm6 4a2 2 0 100-4 2 2 0 000 4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid chart_bar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chart_bar.svg}
#' @examples
#' rheroicons::solid$chart_bar(
#'   id = 'my_chart_bar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chart_bar icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chart_bar <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chart_bar", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 11a1 1 0 011-1h2a1 1 0 011 1v5a1 1 0 01-1 1H3a1 1 0 01-1-1v-5zM8 7a1 1 0 011-1h2a1 1 0 011 1v9a1 1 0 01-1 1H9a1 1 0 01-1-1V7zM14 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1h-2a1 1 0 01-1-1V4z"))))
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
#' @keywords rheroicons solid chart_pie
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chart_pie.svg}
#' @examples
#' rheroicons::solid$chart_pie(
#'   id = 'my_chart_pie_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chart_pie icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chart_pie <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chart_pie", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 10a8 8 0 018-8v8h8a8 8 0 11-16 0z")), 
        tag(`_tag_name` = "path", list(d = "M12 2.252A8.014 8.014 0 0117.748 8H12V2.252z"))))
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
#' @keywords rheroicons solid chart_square_bar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chart_square_bar.svg}
#' @examples
#' rheroicons::solid$chart_square_bar(
#'   id = 'my_chart_square_bar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chart_square_bar icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chart_square_bar <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chart_square_bar", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5 3a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2V5a2 2 0 00-2-2H5zm9 4a1 1 0 10-2 0v6a1 1 0 102 0V7zm-3 2a1 1 0 10-2 0v4a1 1 0 102 0V9zm-3 3a1 1 0 10-2 0v1a1 1 0 102 0v-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid chat_alt_2
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chat_alt_2.svg}
#' @examples
#' rheroicons::solid$chat_alt_2(
#'   id = 'my_chat_alt_2_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chat_alt_2 icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chat_alt_2 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chat_alt_2", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 5a2 2 0 012-2h7a2 2 0 012 2v4a2 2 0 01-2 2H9l-3 3v-3H4a2 2 0 01-2-2V5z")), 
        tag(`_tag_name` = "path", list(d = "M15 7v2a4 4 0 01-4 4H9.828l-1.766 1.767c.28.149.599.233.938.233h2l3 3v-3h2a2 2 0 002-2V9a2 2 0 00-2-2h-1z"))))
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
#' @keywords rheroicons solid chat_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chat_alt.svg}
#' @examples
#' rheroicons::solid$chat_alt(
#'   id = 'my_chat_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chat_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chat_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chat_alt", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 5v8a2 2 0 01-2 2h-5l-5 4v-4H4a2 2 0 01-2-2V5a2 2 0 012-2h12a2 2 0 012 2zM7 8H5v2h2V8zm2 0h2v2H9V8zm6 0h-2v2h2V8z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid chat
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chat.svg}
#' @examples
#' rheroicons::solid$chat(
#'   id = 'my_chat_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chat icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chat <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chat", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 10c0 3.866-3.582 7-8 7a8.841 8.841 0 01-4.083-.98L2 17l1.338-3.123C2.493 12.767 2 11.434 2 10c0-3.866 3.582-7 8-7s8 3.134 8 7zM7 9H5v2h2V9zm8 0h-2v2h2V9zM9 9h2v2H9V9z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid check_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/check_circle.svg}
#' @examples
#' rheroicons::solid$check_circle(
#'   id = 'my_check_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the check_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$check_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_check_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/check.svg}
#' @examples
#' rheroicons::solid$check(
#'   id = 'my_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the check icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$check <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_check", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid chevron_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_down.svg}
#' @examples
#' rheroicons::solid$chevron_down(
#'   id = 'my_chevron_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_down icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chevron_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chevron_down", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid chevron_left
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_left.svg}
#' @examples
#' rheroicons::solid$chevron_left(
#'   id = 'my_chevron_left_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_left icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chevron_left <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chevron_left", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid chevron_right
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_right.svg}
#' @examples
#' rheroicons::solid$chevron_right(
#'   id = 'my_chevron_right_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_right icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chevron_right <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chevron_right", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid chevron_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/chevron_up.svg}
#' @examples
#' rheroicons::solid$chevron_up(
#'   id = 'my_chevron_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the chevron_up icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$chevron_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_chevron_up", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M14.707 12.707a1 1 0 01-1.414 0L10 9.414l-3.293 3.293a1 1 0 01-1.414-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 010 1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid clipboard_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard_check.svg}
#' @examples
#' rheroicons::solid$clipboard_check(
#'   id = 'my_clipboard_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard_check icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$clipboard_check <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_clipboard_check", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M9 2a1 1 0 000 2h2a1 1 0 100-2H9z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v11a2 2 0 01-2 2H6a2 2 0 01-2-2V5zm9.707 5.707a1 1 0 00-1.414-1.414L9 12.586l-1.293-1.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid clipboard_copy
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard_copy.svg}
#' @examples
#' rheroicons::solid$clipboard_copy(
#'   id = 'my_clipboard_copy_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard_copy icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$clipboard_copy <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_clipboard_copy", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M8 2a1 1 0 000 2h2a1 1 0 100-2H8z")), 
        tag(`_tag_name` = "path", list(d = "M3 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v6h-4.586l1.293-1.293a1 1 0 00-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L10.414 13H15v3a2 2 0 01-2 2H5a2 2 0 01-2-2V5zM15 11h2a1 1 0 110 2h-2v-2z"))))
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
#' @keywords rheroicons solid clipboard_list
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard_list.svg}
#' @examples
#' rheroicons::solid$clipboard_list(
#'   id = 'my_clipboard_list_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard_list icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$clipboard_list <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_clipboard_list", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M9 2a1 1 0 000 2h2a1 1 0 100-2H9z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v11a2 2 0 01-2 2H6a2 2 0 01-2-2V5zm3 4a1 1 0 000 2h.01a1 1 0 100-2H7zm3 0a1 1 0 000 2h3a1 1 0 100-2h-3zm-3 4a1 1 0 100 2h.01a1 1 0 100-2H7zm3 0a1 1 0 100 2h3a1 1 0 100-2h-3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid clipboard
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clipboard.svg}
#' @examples
#' rheroicons::solid$clipboard(
#'   id = 'my_clipboard_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clipboard icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$clipboard <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_clipboard", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M8 3a1 1 0 011-1h2a1 1 0 110 2H9a1 1 0 01-1-1z")), 
        tag(`_tag_name` = "path", list(d = "M6 3a2 2 0 00-2 2v11a2 2 0 002 2h8a2 2 0 002-2V5a2 2 0 00-2-2 3 3 0 01-3 3H9a3 3 0 01-3-3z"))))
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
#' @keywords rheroicons solid clock
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/clock.svg}
#' @examples
#' rheroicons::solid$clock(
#'   id = 'my_clock_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the clock icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$clock <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_clock", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-12a1 1 0 10-2 0v4a1 1 0 00.293.707l2.828 2.829a1 1 0 101.415-1.415L11 9.586V6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid cloud_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cloud_download.svg}
#' @examples
#' rheroicons::solid$cloud_download(
#'   id = 'my_cloud_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cloud_download icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$cloud_download <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_cloud_download", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M6 13a4 4 0 010-8 4 4 0 118 0 4 4 0 010 8h-3V8a1 1 0 10-2 0v5H6zM9 13h2v2.586l1.293-1.293a1 1 0 011.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 111.414-1.414L9 15.586V13z"))))
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
#' @keywords rheroicons solid cloud_upload
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cloud_upload.svg}
#' @examples
#' rheroicons::solid$cloud_upload(
#'   id = 'my_cloud_upload_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cloud_upload icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$cloud_upload <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_cloud_upload", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2 10a4 4 0 004 4h3v3a1 1 0 102 0v-3h3a4 4 0 000-8 4 4 0 00-8 0 4 4 0 00-4 4zm9 4H9V9.414l-1.293 1.293a1 1 0 01-1.414-1.414l3-3a1 1 0 011.414 0l3 3a1 1 0 01-1.414 1.414L11 9.414V14z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid code
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/code.svg}
#' @examples
#' rheroicons::solid$code(
#'   id = 'my_code_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the code icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$code <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_code", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M12.316 3.051a1 1 0 01.633 1.265l-4 12a1 1 0 11-1.898-.632l4-12a1 1 0 011.265-.633zM5.707 6.293a1 1 0 010 1.414L3.414 10l2.293 2.293a1 1 0 11-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0zm8.586 0a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 11-1.414-1.414L16.586 10l-2.293-2.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid cog
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cog.svg}
#' @examples
#' rheroicons::solid$cog(
#'   id = 'my_cog_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cog icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$cog <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_cog", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M11.49 3.17c-.38-1.56-2.6-1.56-2.98 0a1.532 1.532 0 01-2.286.948c-1.372-.836-2.942.734-2.106 2.106.54.886.061 2.042-.947 2.287-1.561.379-1.561 2.6 0 2.978a1.532 1.532 0 01.947 2.287c-.836 1.372.734 2.942 2.106 2.106a1.532 1.532 0 012.287.947c.379 1.561 2.6 1.561 2.978 0a1.533 1.533 0 012.287-.947c1.372.836 2.942-.734 2.106-2.106a1.533 1.533 0 01.947-2.287c1.561-.379 1.561-2.6 0-2.978a1.532 1.532 0 01-.947-2.287c.836-1.372-.734-2.942-2.106-2.106a1.532 1.532 0 01-2.287-.947zM10 13a3 3 0 100-6 3 3 0 000 6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid collection
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/collection.svg}
#' @examples
#' rheroicons::solid$collection(
#'   id = 'my_collection_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the collection icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$collection <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_collection", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M7 3a1 1 0 000 2h6a1 1 0 100-2H7zM4 7a1 1 0 011-1h10a1 1 0 110 2H5a1 1 0 01-1-1zM2 11a2 2 0 012-2h12a2 2 0 012 2v4a2 2 0 01-2 2H4a2 2 0 01-2-2v-4z"))))
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
#' @keywords rheroicons solid color_swatch
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/color_swatch.svg}
#' @examples
#' rheroicons::solid$color_swatch(
#'   id = 'my_color_swatch_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the color_swatch icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$color_swatch <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_color_swatch", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 2a2 2 0 00-2 2v11a3 3 0 106 0V4a2 2 0 00-2-2H4zm1 14a1 1 0 100-2 1 1 0 000 2zm5-1.757l4.9-4.9a2 2 0 000-2.828L13.485 5.1a2 2 0 00-2.828 0L10 5.757v8.486zM16 18H9.071l6-6H16a2 2 0 012 2v2a2 2 0 01-2 2z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid credit_card
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/credit_card.svg}
#' @examples
#' rheroicons::solid$credit_card(
#'   id = 'my_credit_card_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the credit_card icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$credit_card <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_credit_card", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M4 4a2 2 0 00-2 2v1h16V6a2 2 0 00-2-2H4z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 9H2v5a2 2 0 002 2h12a2 2 0 002-2V9zM4 13a1 1 0 011-1h1a1 1 0 110 2H5a1 1 0 01-1-1zm5-1a1 1 0 100 2h1a1 1 0 100-2H9z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid currency_dollar
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_dollar.svg}
#' @examples
#' rheroicons::solid$currency_dollar(
#'   id = 'my_currency_dollar_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_dollar icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$currency_dollar <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_currency_dollar", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M8.433 7.418c.155-.103.346-.196.567-.267v1.698a2.305 2.305 0 01-.567-.267C8.07 8.34 8 8.114 8 8c0-.114.07-.34.433-.582zM11 12.849v-1.698c.22.071.412.164.567.267.364.243.433.468.433.582 0 .114-.07.34-.433.582a2.305 2.305 0 01-.567.267z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-13a1 1 0 10-2 0v.092a4.535 4.535 0 00-1.676.662C6.602 6.234 6 7.009 6 8c0 .99.602 1.765 1.324 2.246.48.32 1.054.545 1.676.662v1.941c-.391-.127-.68-.317-.843-.504a1 1 0 10-1.51 1.31c.562.649 1.413 1.076 2.353 1.253V15a1 1 0 102 0v-.092a4.535 4.535 0 001.676-.662C13.398 13.766 14 12.991 14 12c0-.99-.602-1.765-1.324-2.246A4.535 4.535 0 0011 9.092V7.151c.391.127.68.317.843.504a1 1 0 101.511-1.31c-.563-.649-1.413-1.076-2.354-1.253V5z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid currency_euro
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_euro.svg}
#' @examples
#' rheroicons::solid$currency_euro(
#'   id = 'my_currency_euro_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_euro icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$currency_euro <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_currency_euro", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM8.736 6.979C9.208 6.193 9.696 6 10 6c.304 0 .792.193 1.264.979a1 1 0 001.715-1.029C12.279 4.784 11.232 4 10 4s-2.279.784-2.979 1.95c-.285.475-.507 1-.67 1.55H6a1 1 0 000 2h.013a9.358 9.358 0 000 1H6a1 1 0 100 2h.351c.163.55.385 1.075.67 1.55C7.721 15.216 8.768 16 10 16s2.279-.784 2.979-1.95a1 1 0 10-1.715-1.029c-.472.786-.96.979-1.264.979-.304 0-.792-.193-1.264-.979a4.265 4.265 0 01-.264-.521H10a1 1 0 100-2H8.017a7.36 7.36 0 010-1H10a1 1 0 100-2H8.472c.08-.185.167-.36.264-.521z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid currency_pound
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_pound.svg}
#' @examples
#' rheroicons::solid$currency_pound(
#'   id = 'my_currency_pound_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_pound icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$currency_pound <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_currency_pound", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-14a3 3 0 00-3 3v2H7a1 1 0 000 2h1v1a1 1 0 01-1 1 1 1 0 100 2h6a1 1 0 100-2H9.83c.11-.313.17-.65.17-1v-1h1a1 1 0 100-2h-1V7a1 1 0 112 0 1 1 0 102 0 3 3 0 00-3-3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid currency_rupee
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_rupee.svg}
#' @examples
#' rheroicons::solid$currency_rupee(
#'   id = 'my_currency_rupee_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_rupee icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$currency_rupee <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_currency_rupee", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 5a1 1 0 100 2h1a2 2 0 011.732 1H7a1 1 0 100 2h2.732A2 2 0 018 11H7a1 1 0 00-.707 1.707l3 3a1 1 0 001.414-1.414l-1.483-1.484A4.008 4.008 0 0011.874 10H13a1 1 0 100-2h-1.126a3.976 3.976 0 00-.41-1H13a1 1 0 100-2H7z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid currency_yen
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/currency_yen.svg}
#' @examples
#' rheroicons::solid$currency_yen(
#'   id = 'my_currency_yen_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the currency_yen icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$currency_yen <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_currency_yen", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7.858 5.485a1 1 0 00-1.715 1.03L7.633 9H7a1 1 0 100 2h1.834l.166.277V12H7a1 1 0 100 2h2v1a1 1 0 102 0v-1h2a1 1 0 100-2h-2v-.723l.166-.277H13a1 1 0 100-2h-.634l1.492-2.486a1 1 0 10-1.716-1.029L10.034 9h-.068L7.858 5.485z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid cursor_click
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/cursor_click.svg}
#' @examples
#' rheroicons::solid$cursor_click(
#'   id = 'my_cursor_click_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the cursor_click icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$cursor_click <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_cursor_click", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6.672 1.911a1 1 0 10-1.932.518l.259.966a1 1 0 001.932-.518l-.26-.966zM2.429 4.74a1 1 0 10-.517 1.932l.966.259a1 1 0 00.517-1.932l-.966-.26zm8.814-.569a1 1 0 00-1.415-1.414l-.707.707a1 1 0 101.415 1.415l.707-.708zm-7.071 7.072l.707-.707A1 1 0 003.465 9.12l-.708.707a1 1 0 001.415 1.415zm3.2-5.171a1 1 0 00-1.3 1.3l4 10a1 1 0 001.823.075l1.38-2.759 3.018 3.02a1 1 0 001.414-1.415l-3.019-3.02 2.76-1.379a1 1 0 00-.076-1.822l-10-4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid desktop_computer
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/desktop_computer.svg}
#' @examples
#' rheroicons::solid$desktop_computer(
#'   id = 'my_desktop_computer_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the desktop_computer icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$desktop_computer <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_desktop_computer", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 5a2 2 0 012-2h10a2 2 0 012 2v8a2 2 0 01-2 2h-2.22l.123.489.804.804A1 1 0 0113 18H7a1 1 0 01-.707-1.707l.804-.804L7.22 15H5a2 2 0 01-2-2V5zm5.771 7H5V5h10v7H8.771z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid document_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_add.svg}
#' @examples
#' rheroicons::solid$document_add(
#'   id = 'my_document_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_add icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$document_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_document_add", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm5 6a1 1 0 10-2 0v2H7a1 1 0 100 2h2v2a1 1 0 102 0v-2h2a1 1 0 100-2h-2V8z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid document_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_download.svg}
#' @examples
#' rheroicons::solid$document_download(
#'   id = 'my_document_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_download icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$document_download <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_document_download", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm5 6a1 1 0 10-2 0v3.586l-1.293-1.293a1 1 0 10-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L11 11.586V8z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid document_duplicate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_duplicate.svg}
#' @examples
#' rheroicons::solid$document_duplicate(
#'   id = 'my_document_duplicate_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_duplicate icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$document_duplicate <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_document_duplicate", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M9 2a2 2 0 00-2 2v8a2 2 0 002 2h6a2 2 0 002-2V6.414A2 2 0 0016.414 5L14 2.586A2 2 0 0012.586 2H9z")), 
        tag(`_tag_name` = "path", list(d = "M3 8a2 2 0 012-2v10h8a2 2 0 01-2 2H5a2 2 0 01-2-2V8z"))))
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
#' @keywords rheroicons solid document_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_remove.svg}
#' @examples
#' rheroicons::solid$document_remove(
#'   id = 'my_document_remove_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_remove icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$document_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_document_remove", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm1 8a1 1 0 100 2h6a1 1 0 100-2H7z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid document_report
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document_report.svg}
#' @examples
#' rheroicons::solid$document_report(
#'   id = 'my_document_report_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document_report icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$document_report <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_document_report", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M6 2a2 2 0 00-2 2v12a2 2 0 002 2h8a2 2 0 002-2V7.414A2 2 0 0015.414 6L12 2.586A2 2 0 0010.586 2H6zm2 10a1 1 0 10-2 0v3a1 1 0 102 0v-3zm2-3a1 1 0 011 1v5a1 1 0 11-2 0v-5a1 1 0 011-1zm4-1a1 1 0 10-2 0v7a1 1 0 102 0V8z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid document
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/document.svg}
#' @examples
#' rheroicons::solid$document(
#'   id = 'my_document_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the document icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$document <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_document", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 4a2 2 0 012-2h4.586A2 2 0 0112 2.586L15.414 6A2 2 0 0116 7.414V16a2 2 0 01-2 2H6a2 2 0 01-2-2V4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid dots_circle_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/dots_circle_horizontal.svg}
#' @examples
#' rheroicons::solid$dots_circle_horizontal(
#'   id = 'my_dots_circle_horizontal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the dots_circle_horizontal icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$dots_circle_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_dots_circle_horizontal", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9H5v2h2V9zm8 0h-2v2h2V9zM9 9h2v2H9V9z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid dots_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/dots_horizontal.svg}
#' @examples
#' rheroicons::solid$dots_horizontal(
#'   id = 'my_dots_horizontal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the dots_horizontal icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$dots_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_dots_horizontal", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M6 10a2 2 0 11-4 0 2 2 0 014 0zM12 10a2 2 0 11-4 0 2 2 0 014 0zM16 12a2 2 0 100-4 2 2 0 000 4z"))))
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
#' @keywords rheroicons solid dots_vertical
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/dots_vertical.svg}
#' @examples
#' rheroicons::solid$dots_vertical(
#'   id = 'my_dots_vertical_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the dots_vertical icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$dots_vertical <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_dots_vertical", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M10 6a2 2 0 110-4 2 2 0 010 4zM10 12a2 2 0 110-4 2 2 0 010 4zM10 18a2 2 0 110-4 2 2 0 010 4z"))))
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
#' @keywords rheroicons solid download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/download.svg}
#' @examples
#' rheroicons::solid$download(
#'   id = 'my_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the download icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$download <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_download", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 17a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm3.293-7.707a1 1 0 011.414 0L9 10.586V3a1 1 0 112 0v7.586l1.293-1.293a1 1 0 111.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid duplicate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/duplicate.svg}
#' @examples
#' rheroicons::solid$duplicate(
#'   id = 'my_duplicate_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the duplicate icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$duplicate <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_duplicate", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M7 9a2 2 0 012-2h6a2 2 0 012 2v6a2 2 0 01-2 2H9a2 2 0 01-2-2V9z")), 
        tag(`_tag_name` = "path", list(d = "M5 3a2 2 0 00-2 2v6a2 2 0 002 2V5h8a2 2 0 00-2-2H5z"))))
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
#' @keywords rheroicons solid emoji_happy
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/emoji_happy.svg}
#' @examples
#' rheroicons::solid$emoji_happy(
#'   id = 'my_emoji_happy_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the emoji_happy icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$emoji_happy <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_emoji_happy", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9a1 1 0 100-2 1 1 0 000 2zm7-1a1 1 0 11-2 0 1 1 0 012 0zm-.464 5.535a1 1 0 10-1.415-1.414 3 3 0 01-4.242 0 1 1 0 00-1.415 1.414 5 5 0 007.072 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid emoji_sad
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/emoji_sad.svg}
#' @examples
#' rheroicons::solid$emoji_sad(
#'   id = 'my_emoji_sad_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the emoji_sad icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$emoji_sad <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_emoji_sad", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9a1 1 0 100-2 1 1 0 000 2zm7-1a1 1 0 11-2 0 1 1 0 012 0zm-7.536 5.879a1 1 0 001.415 0 3 3 0 014.242 0 1 1 0 001.415-1.415 5 5 0 00-7.072 0 1 1 0 000 1.415z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid exclamation_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/exclamation_circle.svg}
#' @examples
#' rheroicons::solid$exclamation_circle(
#'   id = 'my_exclamation_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the exclamation_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$exclamation_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_exclamation_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid exclamation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/exclamation.svg}
#' @examples
#' rheroicons::solid$exclamation(
#'   id = 'my_exclamation_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the exclamation icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$exclamation <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_exclamation", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid external_link
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/external_link.svg}
#' @examples
#' rheroicons::solid$external_link(
#'   id = 'my_external_link_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the external_link icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$external_link <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_external_link", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z")), 
        tag(`_tag_name` = "path", list(d = "M5 5a2 2 0 00-2 2v8a2 2 0 002 2h8a2 2 0 002-2v-3a1 1 0 10-2 0v3H5V7h3a1 1 0 000-2H5z"))))
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
#' @keywords rheroicons solid eye_off
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/eye_off.svg}
#' @examples
#' rheroicons::solid$eye_off(
#'   id = 'my_eye_off_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the eye_off icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$eye_off <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_eye_off", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3.707 2.293a1 1 0 00-1.414 1.414l14 14a1 1 0 001.414-1.414l-1.473-1.473A10.014 10.014 0 0019.542 10C18.268 5.943 14.478 3 10 3a9.958 9.958 0 00-4.512 1.074l-1.78-1.781zm4.261 4.26l1.514 1.515a2.003 2.003 0 012.45 2.45l1.514 1.514a4 4 0 00-5.478-5.478z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M12.454 16.697L9.75 13.992a4 4 0 01-3.742-3.741L2.335 6.578A9.98 9.98 0 00.458 10c1.274 4.057 5.065 7 9.542 7 .847 0 1.669-.105 2.454-.303z"))))
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
#' @keywords rheroicons solid eye
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/eye.svg}
#' @examples
#' rheroicons::solid$eye(
#'   id = 'my_eye_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the eye icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$eye <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_eye", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M10 12a2 2 0 100-4 2 2 0 000 4z")), tag(`_tag_name` = "path", 
            list(`fill-rule` = "evenodd", d = "M.458 10C1.732 5.943 5.522 3 10 3s8.268 2.943 9.542 7c-1.274 4.057-5.064 7-9.542 7S1.732 14.057.458 10zM14 10a4 4 0 11-8 0 4 4 0 018 0z", 
                `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid filter
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/filter.svg}
#' @examples
#' rheroicons::solid$filter(
#'   id = 'my_filter_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the filter icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$filter <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_filter", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 3a1 1 0 011-1h12a1 1 0 011 1v3a1 1 0 01-.293.707L12 11.414V15a1 1 0 01-.293.707l-2 2A1 1 0 018 17v-5.586L3.293 6.707A1 1 0 013 6V3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid fire
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/fire.svg}
#' @examples
#' rheroicons::solid$fire(
#'   id = 'my_fire_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the fire icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$fire <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_fire", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "g", list(`clip-path` = "url(#clip0)", tag(`_tag_name` = "path", 
            list(stroke = "#374151", `stroke-linecap` = "round", `stroke-linejoin` = "round", 
                `stroke-width` = "2", d = "M14.243 15.243a6 6 0 01-8.486-8.486C5.757 9 6 11 9 12c0-2 1-8 2.5-9 1 2 1.571 2.586 2.742 3.757A5.981 5.981 0 0116 11a5.982 5.982 0 01-1.757 4.243z")), 
            tag(`_tag_name` = "path", list(d = "M7.879 15.121a3 3 0 104.242-4.242C11.536 10.293 11.25 10 10.75 9c-.75.5-1.25 3.5-1.25 4.5C7.879 13.5 7 13 7 13c0 .768.293 1.536.879 2.121z")))), 
        tag(`_tag_name` = "defs", list(tag(`_tag_name` = "clippath", list(id = "clip0", 
            tag(`_tag_name` = "path", list(d = "M0 0h20v20H0z"))))))))
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
#' @keywords rheroicons solid flag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/flag.svg}
#' @examples
#' rheroicons::solid$flag(
#'   id = 'my_flag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the flag icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$flag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_flag", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 6a3 3 0 013-3h10a1 1 0 01.8 1.6L14.25 8l2.55 3.4A1 1 0 0116 13H6a1 1 0 00-1 1v3a1 1 0 11-2 0V6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid folder_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder_add.svg}
#' @examples
#' rheroicons::solid$folder_add(
#'   id = 'my_folder_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder_add icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$folder_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_folder_add", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z")), 
        tag(`_tag_name` = "path", list(stroke = "#fff", `stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M8 11h4m-2-2v4"))))
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
#' @keywords rheroicons solid folder_download
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder_download.svg}
#' @examples
#' rheroicons::solid$folder_download(
#'   id = 'my_folder_download_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder_download icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$folder_download <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_folder_download", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z")), 
        tag(`_tag_name` = "path", list(stroke = "#fff", `stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M10 9v4m0 0l-2-2m2 2l2-2"))))
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
#' @keywords rheroicons solid folder_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder_remove.svg}
#' @examples
#' rheroicons::solid$folder_remove(
#'   id = 'my_folder_remove_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder_remove icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$folder_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_folder_remove", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z")), 
        tag(`_tag_name` = "path", list(stroke = "#fff", `stroke-linecap` = "round", 
            `stroke-linejoin` = "round", `stroke-width` = "2", d = "M8 11h4"))))
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
#' @keywords rheroicons solid folder
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/folder.svg}
#' @examples
#' rheroicons::solid$folder(
#'   id = 'my_folder_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the folder icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$folder <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_folder", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z"))))
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
#' @keywords rheroicons solid globe_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/globe_alt.svg}
#' @examples
#' rheroicons::solid$globe_alt(
#'   id = 'my_globe_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the globe_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$globe_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_globe_alt", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4.083 9h1.946c.089-1.546.383-2.97.837-4.118A6.004 6.004 0 004.083 9zM10 2a8 8 0 100 16 8 8 0 000-16zm0 2c-.076 0-.232.032-.465.262-.238.234-.497.623-.737 1.182-.389.907-.673 2.142-.766 3.556h3.936c-.093-1.414-.377-2.649-.766-3.556-.24-.56-.5-.948-.737-1.182C10.232 4.032 10.076 4 10 4zm3.971 5c-.089-1.546-.383-2.97-.837-4.118A6.004 6.004 0 0115.917 9h-1.946zm-2.003 2H8.032c.093 1.414.377 2.649.766 3.556.24.56.5.948.737 1.182.233.23.389.262.465.262.076 0 .232-.032.465-.262.238-.234.498-.623.737-1.182.389-.907.673-2.142.766-3.556zm1.166 4.118c.454-1.147.748-2.572.837-4.118h1.946a6.004 6.004 0 01-2.783 4.118zm-6.268 0C6.412 13.97 6.118 12.546 6.03 11H4.083a6.004 6.004 0 002.783 4.118z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid globe
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/globe.svg}
#' @examples
#' rheroicons::solid$globe(
#'   id = 'my_globe_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the globe icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$globe <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_globe", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM4.332 8.027a6.012 6.012 0 011.912-2.706C6.512 5.73 6.974 6 7.5 6A1.5 1.5 0 019 7.5V8a2 2 0 004 0 2 2 0 011.523-1.943A5.977 5.977 0 0116 10c0 .34-.028.675-.083 1H15a2 2 0 00-2 2v2.197A5.973 5.973 0 0110 16v-2a2 2 0 00-2-2 2 2 0 01-2-2 2 2 0 00-1.668-1.973z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid hand
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/hand.svg}
#' @examples
#' rheroicons::solid$hand(
#'   id = 'my_hand_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the hand icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$hand <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_hand", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M9 3a1 1 0 012 0v5.5a.5.5 0 001 0V4a1 1 0 112 0v4.5a.5.5 0 001 0V6a1 1 0 112 0v5a7 7 0 11-14 0V9a1 1 0 012 0v2.5a.5.5 0 001 0V4a1 1 0 012 0v4.5a.5.5 0 001 0V3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid hashtag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/hashtag.svg}
#' @examples
#' rheroicons::solid$hashtag(
#'   id = 'my_hashtag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the hashtag icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$hashtag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_hashtag", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M9.243 3.03a1 1 0 01.727 1.213L9.53 6h2.94l.56-2.243a1 1 0 111.94.486L14.53 6H17a1 1 0 110 2h-2.97l-1 4H15a1 1 0 110 2h-2.47l-.56 2.242a1 1 0 11-1.94-.485L10.47 14H7.53l-.56 2.242a1 1 0 11-1.94-.485L5.47 14H3a1 1 0 110-2h2.97l1-4H5a1 1 0 110-2h2.47l.56-2.243a1 1 0 011.213-.727zM9.03 8l-1 4h2.938l1-4H9.031z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid heart
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/heart.svg}
#' @examples
#' rheroicons::solid$heart(
#'   id = 'my_heart_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the heart icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$heart <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_heart", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid home
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/home.svg}
#' @examples
#' rheroicons::solid$home(
#'   id = 'my_home_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the home icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$home <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_home", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M10.707 2.293a1 1 0 00-1.414 0l-7 7a1 1 0 001.414 1.414L4 10.414V17a1 1 0 001 1h2a1 1 0 001-1v-2a1 1 0 011-1h2a1 1 0 011 1v2a1 1 0 001 1h2a1 1 0 001-1v-6.586l.293.293a1 1 0 001.414-1.414l-7-7z"))))
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
#' @keywords rheroicons solid inbox_in
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/inbox_in.svg}
#' @examples
#' rheroicons::solid$inbox_in(
#'   id = 'my_inbox_in_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the inbox_in icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$inbox_in <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_inbox_in", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M8.707 7.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l2-2a1 1 0 00-1.414-1.414L11 7.586V3a1 1 0 10-2 0v4.586l-.293-.293z")), 
        tag(`_tag_name` = "path", list(d = "M3 5a2 2 0 012-2h1a1 1 0 010 2H5v7h2l1 2h4l1-2h2V5h-1a1 1 0 110-2h1a2 2 0 012 2v10a2 2 0 01-2 2H5a2 2 0 01-2-2V5z"))))
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
#' @keywords rheroicons solid inbox
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/inbox.svg}
#' @examples
#' rheroicons::solid$inbox(
#'   id = 'my_inbox_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the inbox icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$inbox <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_inbox", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5 3a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2V5a2 2 0 00-2-2H5zm0 2h10v7h-2l-1 2H8l-1-2H5V5z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid information_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/information_circle.svg}
#' @examples
#' rheroicons::solid$information_circle(
#'   id = 'my_information_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the information_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$information_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_information_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid key
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/key.svg}
#' @examples
#' rheroicons::solid$key(
#'   id = 'my_key_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the key icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$key <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_key", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 8a6 6 0 01-7.743 5.743L10 14l-1 1-1 1H6v2H2v-4l4.257-4.257A6 6 0 1118 8zm-6-4a1 1 0 100 2 2 2 0 012 2 1 1 0 102 0 4 4 0 00-4-4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid library
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/library.svg}
#' @examples
#' rheroicons::solid$library(
#'   id = 'my_library_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the library icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$library <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_library", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10.496 2.132a1 1 0 00-.992 0l-7 4A1 1 0 003 8v7a1 1 0 100 2h14a1 1 0 100-2V8a1 1 0 00.496-1.868l-7-4zM6 9a1 1 0 00-1 1v3a1 1 0 102 0v-3a1 1 0 00-1-1zm3 1a1 1 0 012 0v3a1 1 0 11-2 0v-3zm5-1a1 1 0 00-1 1v3a1 1 0 102 0v-3a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid light_bulb
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/light_bulb.svg}
#' @examples
#' rheroicons::solid$light_bulb(
#'   id = 'my_light_bulb_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the light_bulb icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$light_bulb <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_light_bulb", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M11 3a1 1 0 10-2 0v1a1 1 0 102 0V3zM15.657 5.757a1 1 0 00-1.414-1.414l-.707.707a1 1 0 001.414 1.414l.707-.707zM18 10a1 1 0 01-1 1h-1a1 1 0 110-2h1a1 1 0 011 1zM5.05 6.464A1 1 0 106.464 5.05l-.707-.707a1 1 0 00-1.414 1.414l.707.707zM5 10a1 1 0 01-1 1H3a1 1 0 110-2h1a1 1 0 011 1zM8 16v-1h4v1a2 2 0 11-4 0zM12 14c.015-.34.208-.646.477-.859a4 4 0 10-4.954 0c.27.213.462.519.476.859h4.002z"))))
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
#' @keywords rheroicons solid lightning_bolt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/lightning_bolt.svg}
#' @examples
#' rheroicons::solid$lightning_bolt(
#'   id = 'my_lightning_bolt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the lightning_bolt icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$lightning_bolt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_lightning_bolt", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M11.3 1.046A1 1 0 0112 2v5h4a1 1 0 01.82 1.573l-7 10A1 1 0 018 18v-5H4a1 1 0 01-.82-1.573l7-10a1 1 0 011.12-.38z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid link
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/link.svg}
#' @examples
#' rheroicons::solid$link(
#'   id = 'my_link_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the link icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$link <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_link", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M12.586 4.586a2 2 0 112.828 2.828l-3 3a2 2 0 01-2.828 0 1 1 0 00-1.414 1.414 4 4 0 005.656 0l3-3a4 4 0 00-5.656-5.656l-1.5 1.5a1 1 0 101.414 1.414l1.5-1.5zm-5 5a2 2 0 012.828 0 1 1 0 101.414-1.414 4 4 0 00-5.656 0l-3 3a4 4 0 105.656 5.656l1.5-1.5a1 1 0 10-1.414-1.414l-1.5 1.5a2 2 0 11-2.828-2.828l3-3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid location_marker
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/location_marker.svg}
#' @examples
#' rheroicons::solid$location_marker(
#'   id = 'my_location_marker_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the location_marker icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$location_marker <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_location_marker", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5.05 4.05a7 7 0 119.9 9.9L10 18.9l-4.95-4.95a7 7 0 010-9.9zM10 11a2 2 0 100-4 2 2 0 000 4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid lock_closed
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/lock_closed.svg}
#' @examples
#' rheroicons::solid$lock_closed(
#'   id = 'my_lock_closed_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the lock_closed icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$lock_closed <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_lock_closed", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5 9V7a5 5 0 0110 0v2a2 2 0 012 2v5a2 2 0 01-2 2H5a2 2 0 01-2-2v-5a2 2 0 012-2zm8-2v2H7V7a3 3 0 016 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid lock_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/lock_open.svg}
#' @examples
#' rheroicons::solid$lock_open(
#'   id = 'my_lock_open_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the lock_open icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$lock_open <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_lock_open", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M10 2a5 5 0 00-5 5v2a2 2 0 00-2 2v5a2 2 0 002 2h10a2 2 0 002-2v-5a2 2 0 00-2-2H7V7a3 3 0 015.905-.75 1 1 0 001.937-.5A5.002 5.002 0 0010 2z"))))
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
#' @keywords rheroicons solid logout
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/logout.svg}
#' @examples
#' rheroicons::solid$logout(
#'   id = 'my_logout_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the logout icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$logout <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_logout", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 3a1 1 0 011 1v12a1 1 0 11-2 0V4a1 1 0 011-1zm7.707 3.293a1 1 0 010 1.414L9.414 9H17a1 1 0 110 2H9.414l1.293 1.293a1 1 0 01-1.414 1.414l-3-3a1 1 0 010-1.414l3-3a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid mail_open
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/mail_open.svg}
#' @examples
#' rheroicons::solid$mail_open(
#'   id = 'my_mail_open_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the mail_open icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$mail_open <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_mail_open", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2.94 6.412A2 2 0 002 8.108V16a2 2 0 002 2h12a2 2 0 002-2V8.108a2 2 0 00-.94-1.696l-6-3.75a2 2 0 00-2.12 0l-6 3.75zm2.615 2.423a1 1 0 10-1.11 1.664l5 3.333a1 1 0 001.11 0l5-3.333a1 1 0 00-1.11-1.664L10 11.798 5.555 8.835z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid mail
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/mail.svg}
#' @examples
#' rheroicons::solid$mail(
#'   id = 'my_mail_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the mail icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$mail <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_mail", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2.003 5.884L10 9.882l7.997-3.998A2 2 0 0016 4H4a2 2 0 00-1.997 1.884z")), 
        tag(`_tag_name` = "path", list(d = "M18 8.118l-8 4-8-4V14a2 2 0 002 2h12a2 2 0 002-2V8.118z"))))
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
#' @keywords rheroicons solid menu_alt_1
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_1.svg}
#' @examples
#' rheroicons::solid$menu_alt_1(
#'   id = 'my_menu_alt_1_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_1 icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$menu_alt_1 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_menu_alt_1", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h6a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid menu_alt_2
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_2.svg}
#' @examples
#' rheroicons::solid$menu_alt_2(
#'   id = 'my_menu_alt_2_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_2 icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$menu_alt_2 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_menu_alt_2", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h6a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid menu_alt_3
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_3.svg}
#' @examples
#' rheroicons::solid$menu_alt_3(
#'   id = 'my_menu_alt_3_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_3 icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$menu_alt_3 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_menu_alt_3", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM9 15a1 1 0 011-1h6a1 1 0 110 2h-6a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid menu_alt_4
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu_alt_4.svg}
#' @examples
#' rheroicons::solid$menu_alt_4(
#'   id = 'my_menu_alt_4_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu_alt_4 icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$menu_alt_4 <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_menu_alt_4", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 7a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 13a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid menu
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/menu.svg}
#' @examples
#' rheroicons::solid$menu(
#'   id = 'my_menu_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the menu icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$menu <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_menu", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid microphone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/microphone.svg}
#' @examples
#' rheroicons::solid$microphone(
#'   id = 'my_microphone_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the microphone icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$microphone <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_microphone", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M7 4a3 3 0 016 0v4a3 3 0 11-6 0V4zm4 10.93A7.001 7.001 0 0017 8a1 1 0 10-2 0A5 5 0 015 8a1 1 0 00-2 0 7.001 7.001 0 006 6.93V17H6a1 1 0 100 2h8a1 1 0 100-2h-3v-2.07z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid minus_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/minus_circle.svg}
#' @examples
#' rheroicons::solid$minus_circle(
#'   id = 'my_minus_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the minus_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$minus_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_minus_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM7 9a1 1 0 000 2h6a1 1 0 100-2H7z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid moon
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/moon.svg}
#' @examples
#' rheroicons::solid$moon(
#'   id = 'my_moon_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the moon icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$moon <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_moon", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"))))
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
#' @keywords rheroicons solid newspaper
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/newspaper.svg}
#' @examples
#' rheroicons::solid$newspaper(
#'   id = 'my_newspaper_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the newspaper icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$newspaper <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_newspaper", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2 5a2 2 0 012-2h8a2 2 0 012 2v10a2 2 0 002 2H4a2 2 0 01-2-2V5zm3 1h6v4H5V6zm6 6H5v2h6v-2z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M15 7h1a2 2 0 012 2v5.5a1.5 1.5 0 01-3 0V7z"))))
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
#' @keywords rheroicons solid office_building
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/office_building.svg}
#' @examples
#' rheroicons::solid$office_building(
#'   id = 'my_office_building_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the office_building icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$office_building <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_office_building", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 4a2 2 0 012-2h8a2 2 0 012 2v12a1 1 0 110 2h-3a1 1 0 01-1-1v-2a1 1 0 00-1-1H9a1 1 0 00-1 1v2a1 1 0 01-1 1H4a1 1 0 110-2V4zm3 1h2v2H7V5zm2 4H7v2h2V9zm2-4h2v2h-2V5zm2 4h-2v2h2V9z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid paper_clip
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/paper_clip.svg}
#' @examples
#' rheroicons::solid$paper_clip(
#'   id = 'my_paper_clip_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the paper_clip icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$paper_clip <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_paper_clip", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M8 4a3 3 0 00-3 3v4a5 5 0 0010 0V7a1 1 0 112 0v4a7 7 0 11-14 0V7a5 5 0 0110 0v4a3 3 0 11-6 0V7a1 1 0 012 0v4a1 1 0 102 0V7a3 3 0 00-3-3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid pause
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/pause.svg}
#' @examples
#' rheroicons::solid$pause(
#'   id = 'my_pause_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the pause icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$pause <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_pause", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zM7 8a1 1 0 012 0v4a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v4a1 1 0 102 0V8a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid pencil_alt
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/pencil_alt.svg}
#' @examples
#' rheroicons::solid$pencil_alt(
#'   id = 'my_pencil_alt_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the pencil_alt icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$pencil_alt <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_pencil_alt", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M17.414 2.586a2 2 0 00-2.828 0L7 10.172V13h2.828l7.586-7.586a2 2 0 000-2.828z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2 6a2 2 0 012-2h4a1 1 0 010 2H4v10h10v-4a1 1 0 112 0v4a2 2 0 01-2 2H4a2 2 0 01-2-2V6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid pencil
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/pencil.svg}
#' @examples
#' rheroicons::solid$pencil(
#'   id = 'my_pencil_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the pencil icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$pencil <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_pencil", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M13.586 3.586a2 2 0 112.828 2.828l-.793.793-2.828-2.828.793-.793zM11.379 5.793L3 14.172V17h2.828l8.38-8.379-2.83-2.828z"))))
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
#' @keywords rheroicons solid phone_incoming
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/phone_incoming.svg}
#' @examples
#' rheroicons::solid$phone_incoming(
#'   id = 'my_phone_incoming_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the phone_incoming icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$phone_incoming <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_phone_incoming", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M14.414 7l3.293-3.293a1 1 0 00-1.414-1.414L13 5.586V4a1 1 0 10-2 0v4.003a.996.996 0 00.617.921A.997.997 0 0012 9h4a1 1 0 100-2h-1.586z")), 
        tag(`_tag_name` = "path", list(d = "M2 3a1 1 0 011-1h2.153a1 1 0 01.986.836l.74 4.435a1 1 0 01-.54 1.06l-1.548.773a11.037 11.037 0 006.105 6.105l.774-1.548a1 1 0 011.059-.54l4.435.74a1 1 0 01.836.986V17a1 1 0 01-1 1h-2C7.82 18 2 12.18 2 5V3z"))))
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
#' @keywords rheroicons solid phone_outgoing
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/phone_outgoing.svg}
#' @examples
#' rheroicons::solid$phone_outgoing(
#'   id = 'my_phone_outgoing_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the phone_outgoing icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$phone_outgoing <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_phone_outgoing", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M17.924 2.617a.997.997 0 00-.215-.322l-.004-.004A.997.997 0 0017 2h-4a1 1 0 100 2h1.586l-3.293 3.293a1 1 0 001.414 1.414L16 5.414V7a1 1 0 102 0V3a.997.997 0 00-.076-.383z")), 
        tag(`_tag_name` = "path", list(d = "M2 3a1 1 0 011-1h2.153a1 1 0 01.986.836l.74 4.435a1 1 0 01-.54 1.06l-1.548.773a11.037 11.037 0 006.105 6.105l.774-1.548a1 1 0 011.059-.54l4.435.74a1 1 0 01.836.986V17a1 1 0 01-1 1h-2C7.82 18 2 12.18 2 5V3z"))))
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
#' @keywords rheroicons solid phone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/phone.svg}
#' @examples
#' rheroicons::solid$phone(
#'   id = 'my_phone_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the phone icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$phone <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_phone", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 3a1 1 0 011-1h2.153a1 1 0 01.986.836l.74 4.435a1 1 0 01-.54 1.06l-1.548.773a11.037 11.037 0 006.105 6.105l.774-1.548a1 1 0 011.059-.54l4.435.74a1 1 0 01.836.986V17a1 1 0 01-1 1h-2C7.82 18 2 12.18 2 5V3z"))))
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
#' @keywords rheroicons solid photograph
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/photograph.svg}
#' @examples
#' rheroicons::solid$photograph(
#'   id = 'my_photograph_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the photograph icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$photograph <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_photograph", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 3a2 2 0 00-2 2v10a2 2 0 002 2h12a2 2 0 002-2V5a2 2 0 00-2-2H4zm12 12H4l4-8 3 6 2-4 3 6z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid play
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/play.svg}
#' @examples
#' rheroicons::solid$play(
#'   id = 'my_play_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the play icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$play <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_play", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM9.555 7.168A1 1 0 008 8v4a1 1 0 001.555.832l3-2a1 1 0 000-1.664l-3-2z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid plus_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/plus_circle.svg}
#' @examples
#' rheroicons::solid$plus_circle(
#'   id = 'my_plus_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the plus_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$plus_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_plus_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zm1-11a1 1 0 10-2 0v2H7a1 1 0 100 2h2v2a1 1 0 102 0v-2h2a1 1 0 100-2h-2V7z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid plus
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/plus.svg}
#' @examples
#' rheroicons::solid$plus(
#'   id = 'my_plus_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the plus icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$plus <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_plus", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid printer
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/printer.svg}
#' @examples
#' rheroicons::solid$printer(
#'   id = 'my_printer_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the printer icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$printer <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_printer", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5 4v3H4a2 2 0 00-2 2v3a2 2 0 002 2h1v2a2 2 0 002 2h6a2 2 0 002-2v-2h1a2 2 0 002-2V9a2 2 0 00-2-2h-1V4a2 2 0 00-2-2H7a2 2 0 00-2 2zm8 0H7v3h6V4zm0 8H7v4h6v-4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid puzzle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/puzzle.svg}
#' @examples
#' rheroicons::solid$puzzle(
#'   id = 'my_puzzle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the puzzle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$puzzle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_puzzle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M10 3.5a1.5 1.5 0 013 0V4a1 1 0 001 1h3a1 1 0 011 1v3a1 1 0 01-1 1h-.5a1.5 1.5 0 000 3h.5a1 1 0 011 1v3a1 1 0 01-1 1h-3a1 1 0 01-1-1v-.5a1.5 1.5 0 00-3 0v.5a1 1 0 01-1 1H6a1 1 0 01-1-1v-3a1 1 0 00-1-1h-.5a1.5 1.5 0 010-3H4a1 1 0 001-1V6a1 1 0 011-1h3a1 1 0 001-1v-.5z"))))
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
#' @keywords rheroicons solid qrcode
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/qrcode.svg}
#' @examples
#' rheroicons::solid$qrcode(
#'   id = 'my_qrcode_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the qrcode icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$qrcode <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_qrcode", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 4a1 1 0 011-1h3a1 1 0 011 1v3a1 1 0 01-1 1H4a1 1 0 01-1-1V4zm2 2V5h1v1H5zM3 13a1 1 0 011-1h3a1 1 0 011 1v3a1 1 0 01-1 1H4a1 1 0 01-1-1v-3zm2 2v-1h1v1H5zM13 3a1 1 0 00-1 1v3a1 1 0 001 1h3a1 1 0 001-1V4a1 1 0 00-1-1h-3zm1 2v1h1V5h-1z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(d = "M11 4a1 1 0 10-2 0v1a1 1 0 002 0V4zM10 7a1 1 0 011 1v1h2a1 1 0 110 2h-3a1 1 0 01-1-1V8a1 1 0 011-1zM16 9a1 1 0 100 2 1 1 0 000-2zM9 13a1 1 0 011-1h1a1 1 0 110 2v2a1 1 0 11-2 0v-3zM7 11a1 1 0 100-2H4a1 1 0 100 2h3zM17 13a1 1 0 01-1 1h-2a1 1 0 110-2h2a1 1 0 011 1zM16 17a1 1 0 100-2h-3a1 1 0 100 2h3z"))))
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
#' @keywords rheroicons solid question_mark_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/question_mark_circle.svg}
#' @examples
#' rheroicons::solid$question_mark_circle(
#'   id = 'my_question_mark_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the question_mark_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$question_mark_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_question_mark_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid receipt_refund
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/receipt_refund.svg}
#' @examples
#' rheroicons::solid$receipt_refund(
#'   id = 'my_receipt_refund_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the receipt_refund icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$receipt_refund <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_receipt_refund", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5 2a2 2 0 00-2 2v14l3.5-2 3.5 2 3.5-2 3.5 2V4a2 2 0 00-2-2H5zm4.707 3.707a1 1 0 00-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L8.414 9H10a3 3 0 013 3v1a1 1 0 102 0v-1a5 5 0 00-5-5H8.414l1.293-1.293z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid refresh
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/refresh.svg}
#' @examples
#' rheroicons::solid$refresh(
#'   id = 'my_refresh_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the refresh icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$refresh <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_refresh", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4 2a1 1 0 011 1v2.101a7.002 7.002 0 0111.601 2.566 1 1 0 11-1.885.666A5.002 5.002 0 005.999 7H9a1 1 0 010 2H4a1 1 0 01-1-1V3a1 1 0 011-1zm.008 9.057a1 1 0 011.276.61A5.002 5.002 0 0014.001 13H11a1 1 0 110-2h5a1 1 0 011 1v5a1 1 0 11-2 0v-2.101a7.002 7.002 0 01-11.601-2.566 1 1 0 01.61-1.276z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid reply
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/reply.svg}
#' @examples
#' rheroicons::solid$reply(
#'   id = 'my_reply_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the reply icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$reply <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_reply", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M7.707 3.293a1 1 0 010 1.414L5.414 7H11a7 7 0 017 7v2a1 1 0 11-2 0v-2a5 5 0 00-5-5H5.414l2.293 2.293a1 1 0 11-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid scale
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/scale.svg}
#' @examples
#' rheroicons::solid$scale(
#'   id = 'my_scale_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the scale icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$scale <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_scale", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 2a1 1 0 011 1v1.323l3.954 1.582 1.599-.8a1 1 0 01.894 1.79l-1.233.616 1.738 5.42a1 1 0 01-.285 1.05A3.989 3.989 0 0115 15a3.989 3.989 0 01-2.667-1.019 1 1 0 01-.285-1.05l1.715-5.349L11 6.477V16h2a1 1 0 110 2H7a1 1 0 110-2h2V6.477L6.237 7.582l1.715 5.349a1 1 0 01-.285 1.05A3.989 3.989 0 015 15a3.989 3.989 0 01-2.667-1.019 1 1 0 01-.285-1.05l1.738-5.42-1.233-.617a1 1 0 01.894-1.788l1.599.799L9 4.323V3a1 1 0 011-1zm-5 8.274l-.818 2.552c.25.112.526.174.818.174.292 0 .569-.062.818-.174L5 10.274zm10 0l-.818 2.552c.25.112.526.174.818.174.292 0 .569-.062.818-.174L15 10.274z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid search
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/search.svg}
#' @examples
#' rheroicons::solid$search(
#'   id = 'my_search_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the search icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$search <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_search", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid selector
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/selector.svg}
#' @examples
#' rheroicons::solid$selector(
#'   id = 'my_selector_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the selector icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$selector <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_selector", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 3a1 1 0 01.707.293l3 3a1 1 0 01-1.414 1.414L10 5.414 7.707 7.707a1 1 0 01-1.414-1.414l3-3A1 1 0 0110 3zm-3.707 9.293a1 1 0 011.414 0L10 14.586l2.293-2.293a1 1 0 011.414 1.414l-3 3a1 1 0 01-1.414 0l-3-3a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid share
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/share.svg}
#' @examples
#' rheroicons::solid$share(
#'   id = 'my_share_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the share icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$share <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_share", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M15 8a3 3 0 10-2.977-2.63l-4.94 2.47a3 3 0 100 4.319l4.94 2.47a3 3 0 10.895-1.789l-4.94-2.47a3.027 3.027 0 000-.74l4.94-2.47C13.456 7.68 14.19 8 15 8z"))))
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
#' @keywords rheroicons solid shield_check
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shield_check.svg}
#' @examples
#' rheroicons::solid$shield_check(
#'   id = 'my_shield_check_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shield_check icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$shield_check <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_shield_check", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2.166 4.999A11.954 11.954 0 0010 1.944 11.954 11.954 0 0017.834 5c.11.65.166 1.32.166 2.001 0 5.225-3.34 9.67-8 11.317C5.34 16.67 2 12.225 2 7c0-.682.057-1.35.166-2.001zm11.541 3.708a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid shield_exclamation
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shield_exclamation.svg}
#' @examples
#' rheroicons::solid$shield_exclamation(
#'   id = 'my_shield_exclamation_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shield_exclamation icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$shield_exclamation <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_shield_exclamation", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 1.944A11.954 11.954 0 012.166 5C2.056 5.649 2 6.319 2 7c0 5.225 3.34 9.67 8 11.317C14.66 16.67 18 12.225 18 7c0-.682-.057-1.35-.166-2.001A11.954 11.954 0 0110 1.944zM11 14a1 1 0 11-2 0 1 1 0 012 0zm0-7a1 1 0 10-2 0v3a1 1 0 102 0V7z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid shopping_bag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shopping_bag.svg}
#' @examples
#' rheroicons::solid$shopping_bag(
#'   id = 'my_shopping_bag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shopping_bag icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$shopping_bag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_shopping_bag", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(stroke = "#374151", `stroke-linejoin` = "round", 
            `stroke-width` = "2", d = "M5 8h10l1 9H4l1-9z")), tag(`_tag_name` = "path", 
            list(stroke = "#374151", `stroke-width` = "2", d = "M7 6a3 3 0 013-3v0a3 3 0 013 3v3a3 3 0 01-3 3v0a3 3 0 01-3-3V6z")), 
        tag(`_tag_name` = "rect", list(width = "2", height = "2", x = "6", y = "9", 
            rx = "1")), tag(`_tag_name` = "rect", list(width = "2", height = "2", 
            x = "12", y = "9", rx = "1"))))
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
#' @keywords rheroicons solid shopping_cart
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/shopping_cart.svg}
#' @examples
#' rheroicons::solid$shopping_cart(
#'   id = 'my_shopping_cart_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the shopping_cart icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$shopping_cart <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_shopping_cart", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M3 1a1 1 0 000 2h1.22l.305 1.222a.997.997 0 00.01.042l1.358 5.43-.893.892C3.74 11.846 4.632 14 6.414 14H15a1 1 0 000-2H6.414l1-1H14a1 1 0 00.894-.553l3-6A1 1 0 0017 3H6.28l-.31-1.243A1 1 0 005 1H3zM16 16.5a1.5 1.5 0 11-3 0 1.5 1.5 0 013 0zM6.5 18a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"))))
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
#' @keywords rheroicons solid sort_ascending
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sort_ascending.svg}
#' @examples
#' rheroicons::solid$sort_ascending(
#'   id = 'my_sort_ascending_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sort_ascending icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$sort_ascending <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_sort_ascending", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M3 3a1 1 0 000 2h11a1 1 0 100-2H3zM3 7a1 1 0 000 2h5a1 1 0 000-2H3zM3 11a1 1 0 100 2h4a1 1 0 100-2H3zM13 16a1 1 0 102 0v-5.586l1.293 1.293a1 1 0 001.414-1.414l-3-3a1 1 0 00-1.414 0l-3 3a1 1 0 101.414 1.414L13 10.414V16z"))))
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
#' @keywords rheroicons solid sort_descending
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sort_descending.svg}
#' @examples
#' rheroicons::solid$sort_descending(
#'   id = 'my_sort_descending_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sort_descending icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$sort_descending <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_sort_descending", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M3 3a1 1 0 000 2h11a1 1 0 100-2H3zM3 7a1 1 0 000 2h7a1 1 0 100-2H3zM3 11a1 1 0 100 2h4a1 1 0 100-2H3zM15 8a1 1 0 10-2 0v5.586l-1.293-1.293a1 1 0 00-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L15 13.586V8z"))))
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
#' @keywords rheroicons solid sparkles
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sparkles.svg}
#' @examples
#' rheroicons::solid$sparkles(
#'   id = 'my_sparkles_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sparkles icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$sparkles <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_sparkles", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M5 2a1 1 0 011 1v1h1a1 1 0 010 2H6v1a1 1 0 01-2 0V6H3a1 1 0 010-2h1V3a1 1 0 011-1zm0 10a1 1 0 011 1v1h1a1 1 0 110 2H6v1a1 1 0 11-2 0v-1H3a1 1 0 110-2h1v-1a1 1 0 011-1zM12 2a1 1 0 01.967.744L14.146 7.2 17.5 9.134a1 1 0 010 1.732l-3.354 1.935-1.18 4.455a1 1 0 01-1.933 0L9.854 12.8 6.5 10.866a1 1 0 010-1.732l3.354-1.935 1.18-4.455A1 1 0 0112 2z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid speakerphone
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/speakerphone.svg}
#' @examples
#' rheroicons::solid$speakerphone(
#'   id = 'my_speakerphone_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the speakerphone icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$speakerphone <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_speakerphone", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 3a1 1 0 00-1.447-.894L8.763 6H5a3 3 0 000 6h.28l1.771 5.316A1 1 0 008 18h1a1 1 0 001-1v-4.382l6.553 3.276A1 1 0 0018 15V3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid star
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/star.svg}
#' @examples
#' rheroicons::solid$star(
#'   id = 'my_star_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the star icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$star <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_star", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z"))))
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
#' @keywords rheroicons solid stop
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/stop.svg}
#' @examples
#' rheroicons::solid$stop(
#'   id = 'my_stop_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the stop icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$stop <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_stop", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM8 7a1 1 0 00-1 1v4a1 1 0 001 1h4a1 1 0 001-1V8a1 1 0 00-1-1H8z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid sun
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/sun.svg}
#' @examples
#' rheroicons::solid$sun(
#'   id = 'my_sun_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the sun icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$sun <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_sun", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid support
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/support.svg}
#' @examples
#' rheroicons::solid$support(
#'   id = 'my_support_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the support icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$support <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_support", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-2 0c0 .993-.241 1.929-.668 2.754l-1.524-1.525a3.997 3.997 0 00.078-2.183l1.562-1.562C15.802 8.249 16 9.1 16 10zm-5.165 3.913l1.58 1.58A5.98 5.98 0 0110 16a5.976 5.976 0 01-2.516-.552l1.562-1.562a4.006 4.006 0 001.789.027zm-4.677-2.796a4.002 4.002 0 01-.041-2.08l-.08.08-1.53-1.533A5.98 5.98 0 004 10c0 .954.223 1.856.619 2.657l1.54-1.54zm1.088-6.45A5.974 5.974 0 0110 4c.954 0 1.856.223 2.657.619l-1.54 1.54a4.002 4.002 0 00-2.346.033L7.246 4.668zM12 10a2 2 0 11-4 0 2 2 0 014 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid switch_horizontal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/switch_horizontal.svg}
#' @examples
#' rheroicons::solid$switch_horizontal(
#'   id = 'my_switch_horizontal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the switch_horizontal icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$switch_horizontal <- function(id = NULL, class = NULL, aria_hidden = FALSE, 
    title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_switch_horizontal", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M8 5a1 1 0 100 2h5.586l-1.293 1.293a1 1 0 001.414 1.414l3-3a1 1 0 000-1.414l-3-3a1 1 0 10-1.414 1.414L13.586 5H8zM12 15a1 1 0 100-2H6.414l1.293-1.293a1 1 0 10-1.414-1.414l-3 3a1 1 0 000 1.414l3 3a1 1 0 001.414-1.414L6.414 15H12z"))))
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
#' @keywords rheroicons solid switch_vertical
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/switch_vertical.svg}
#' @examples
#' rheroicons::solid$switch_vertical(
#'   id = 'my_switch_vertical_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the switch_vertical icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$switch_vertical <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_switch_vertical", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M5 12a1 1 0 102 0V6.414l1.293 1.293a1 1 0 001.414-1.414l-3-3a1 1 0 00-1.414 0l-3 3a1 1 0 001.414 1.414L5 6.414V12zM15 8a1 1 0 10-2 0v5.586l-1.293-1.293a1 1 0 00-1.414 1.414l3 3a1 1 0 001.414 0l3-3a1 1 0 00-1.414-1.414L15 13.586V8z"))))
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
#' @keywords rheroicons solid tag
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/tag.svg}
#' @examples
#' rheroicons::solid$tag(
#'   id = 'my_tag_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the tag icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$tag <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_tag", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M17.707 9.293a1 1 0 010 1.414l-7 7a1 1 0 01-1.414 0l-7-7A.997.997 0 012 10V5a3 3 0 013-3h5c.256 0 .512.098.707.293l7 7zM5 6a1 1 0 100-2 1 1 0 000 2z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid template
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/template.svg}
#' @examples
#' rheroicons::solid$template(
#'   id = 'my_template_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the template icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$template <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_template", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M3 4a1 1 0 011-1h12a1 1 0 011 1v2a1 1 0 01-1 1H4a1 1 0 01-1-1V4zM3 10a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H4a1 1 0 01-1-1v-6zM14 9a1 1 0 00-1 1v6a1 1 0 001 1h2a1 1 0 001-1v-6a1 1 0 00-1-1h-2z"))))
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
#' @keywords rheroicons solid terminal
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/terminal.svg}
#' @examples
#' rheroicons::solid$terminal(
#'   id = 'my_terminal_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the terminal icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$terminal <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_terminal", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2 5a2 2 0 012-2h12a2 2 0 012 2v10a2 2 0 01-2 2H4a2 2 0 01-2-2V5zm3.293 1.293a1 1 0 011.414 0l3 3a1 1 0 010 1.414l-3 3a1 1 0 01-1.414-1.414L7.586 10 5.293 7.707a1 1 0 010-1.414zM11 12a1 1 0 100 2h3a1 1 0 100-2h-3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid thumb_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/thumb_down.svg}
#' @examples
#' rheroicons::solid$thumb_down(
#'   id = 'my_thumb_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the thumb_down icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$thumb_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_thumb_down", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M18 9.5a1.5 1.5 0 11-3 0v-6a1.5 1.5 0 013 0v6zM14 9.667v-5.43a2 2 0 00-1.105-1.79l-.05-.025A4 4 0 0011.055 2H5.64a2 2 0 00-1.962 1.608l-1.2 6A2 2 0 004.44 12H8v4a2 2 0 002 2 1 1 0 001-1v-.667a4 4 0 01.8-2.4l1.4-1.866a4 4 0 00.8-2.4z"))))
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
#' @keywords rheroicons solid thumb_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/thumb_up.svg}
#' @examples
#' rheroicons::solid$thumb_up(
#'   id = 'my_thumb_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the thumb_up icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$thumb_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_thumb_up", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 10.5a1.5 1.5 0 113 0v6a1.5 1.5 0 01-3 0v-6zM6 10.333v5.43a2 2 0 001.106 1.79l.05.025A4 4 0 008.943 18h5.416a2 2 0 001.962-1.608l1.2-6A2 2 0 0015.56 8H12V4a2 2 0 00-2-2 1 1 0 00-1 1v.667a4 4 0 01-.8 2.4L6.8 7.933a4 4 0 00-.8 2.4z"))))
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
#' @keywords rheroicons solid ticket
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/ticket.svg}
#' @examples
#' rheroicons::solid$ticket(
#'   id = 'my_ticket_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the ticket icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$ticket <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_ticket", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 6a2 2 0 012-2h12a2 2 0 012 2v2a2 2 0 100 4v2a2 2 0 01-2 2H4a2 2 0 01-2-2v-2a2 2 0 100-4V6z"))))
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
#' @keywords rheroicons solid translate
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/translate.svg}
#' @examples
#' rheroicons::solid$translate(
#'   id = 'my_translate_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the translate icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$translate <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_translate", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M7 2a1 1 0 011 1v1h3a1 1 0 110 2H9.578a18.87 18.87 0 01-1.724 4.78c.29.354.596.696.914 1.026a1 1 0 11-1.44 1.389c-.188-.196-.373-.396-.554-.6a19.098 19.098 0 01-3.107 3.567 1 1 0 01-1.334-1.49 17.087 17.087 0 003.13-3.733 18.992 18.992 0 01-1.487-2.494 1 1 0 111.79-.89c.234.47.489.928.764 1.372.417-.934.752-1.913.997-2.927H3a1 1 0 110-2h3V3a1 1 0 011-1zm6 6a1 1 0 01.894.553l2.991 5.982a.869.869 0 01.02.037l.99 1.98a1 1 0 11-1.79.895L15.383 16h-4.764l-.724 1.447a1 1 0 11-1.788-.894l.99-1.98.019-.038 2.99-5.982A1 1 0 0113 8zm-1.382 6h2.764L13 11.236 11.618 14z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid trash
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/trash.svg}
#' @examples
#' rheroicons::solid$trash(
#'   id = 'my_trash_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the trash icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$trash <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_trash", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid trending_down
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/trending_down.svg}
#' @examples
#' rheroicons::solid$trending_down(
#'   id = 'my_trending_down_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the trending_down icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$trending_down <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_trending_down", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M12 13a1 1 0 100 2h5a1 1 0 001-1V9a1 1 0 10-2 0v2.586l-4.293-4.293a1 1 0 00-1.414 0L8 9.586 3.707 5.293a1 1 0 00-1.414 1.414l5 5a1 1 0 001.414 0L11 9.414 14.586 13H12z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid trending_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/trending_up.svg}
#' @examples
#' rheroicons::solid$trending_up(
#'   id = 'my_trending_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the trending_up icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$trending_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_trending_up", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M12 7a1 1 0 110-2h5a1 1 0 011 1v5a1 1 0 11-2 0V8.414l-4.293 4.293a1 1 0 01-1.414 0L8 10.414l-4.293 4.293a1 1 0 01-1.414-1.414l5-5a1 1 0 011.414 0L11 10.586 14.586 7H12z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid upload
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/upload.svg}
#' @examples
#' rheroicons::solid$upload(
#'   id = 'my_upload_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the upload icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$upload <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_upload", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 17a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM6.293 6.707a1 1 0 010-1.414l3-3a1 1 0 011.414 0l3 3a1 1 0 01-1.414 1.414L11 5.414V13a1 1 0 11-2 0V5.414L7.707 6.707a1 1 0 01-1.414 0z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid user_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_add.svg}
#' @examples
#' rheroicons::solid$user_add(
#'   id = 'my_user_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_add icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$user_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_user_add", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M8 9a3 3 0 100-6 3 3 0 000 6zM8 11a6 6 0 016 6H2a6 6 0 016-6zM16 7a1 1 0 10-2 0v1h-1a1 1 0 100 2h1v1a1 1 0 102 0v-1h1a1 1 0 100-2h-1V7z"))))
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
#' @keywords rheroicons solid user_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_circle.svg}
#' @examples
#' rheroicons::solid$user_circle(
#'   id = 'my_user_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$user_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_user_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-6-3a2 2 0 11-4 0 2 2 0 014 0zm-2 4a5 5 0 00-4.546 2.916A5.986 5.986 0 0010 16a5.986 5.986 0 004.546-2.084A5 5 0 0010 11z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid user_group
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_group.svg}
#' @examples
#' rheroicons::solid$user_group(
#'   id = 'my_user_group_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_group icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$user_group <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_user_group", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M13 6a3 3 0 11-6 0 3 3 0 016 0zM18 8a2 2 0 11-4 0 2 2 0 014 0zM14 15a4 4 0 00-8 0v3h8v-3zM6 8a2 2 0 11-4 0 2 2 0 014 0zM16 18v-3a5.972 5.972 0 00-.75-2.906A3.005 3.005 0 0119 15v3h-3zM4.75 12.094A5.973 5.973 0 004 15v3H1v-3a3 3 0 013.75-2.906z"))))
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
#' @keywords rheroicons solid user_remove
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user_remove.svg}
#' @examples
#' rheroicons::solid$user_remove(
#'   id = 'my_user_remove_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user_remove icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$user_remove <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_user_remove", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M11 6a3 3 0 11-6 0 3 3 0 016 0zM14 17a6 6 0 00-12 0h12zM13 8a1 1 0 100 2h4a1 1 0 100-2h-4z"))))
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
#' @keywords rheroicons solid user
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/user.svg}
#' @examples
#' rheroicons::solid$user(
#'   id = 'my_user_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the user icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$user <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_user", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 9a3 3 0 100-6 3 3 0 000 6zm-7 9a7 7 0 1114 0H3z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid users
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/users.svg}
#' @examples
#' rheroicons::solid$users(
#'   id = 'my_users_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the users icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$users <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_users", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M9 6a3 3 0 11-6 0 3 3 0 016 0zM17 6a3 3 0 11-6 0 3 3 0 016 0zM12.93 17c.046-.327.07-.66.07-1a6.97 6.97 0 00-1.5-4.33A5 5 0 0119 16v1h-6.07zM6 11a5 5 0 015 5v1H1v-1a5 5 0 015-5z"))))
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
#' @keywords rheroicons solid view_boards
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_boards.svg}
#' @examples
#' rheroicons::solid$view_boards(
#'   id = 'my_view_boards_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_boards icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$view_boards <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_view_boards", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M2 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1H3a1 1 0 01-1-1V4zM8 4a1 1 0 011-1h2a1 1 0 011 1v12a1 1 0 01-1 1H9a1 1 0 01-1-1V4zM15 3a1 1 0 00-1 1v12a1 1 0 001 1h2a1 1 0 001-1V4a1 1 0 00-1-1h-2z"))))
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
#' @keywords rheroicons solid view_grid_add
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_grid_add.svg}
#' @examples
#' rheroicons::solid$view_grid_add(
#'   id = 'my_view_grid_add_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_grid_add icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$view_grid_add <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_view_grid_add", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM14 11a1 1 0 011 1v1h1a1 1 0 110 2h-1v1a1 1 0 11-2 0v-1h-1a1 1 0 110-2h1v-1a1 1 0 011-1z"))))
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
#' @keywords rheroicons solid view_grid
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_grid.svg}
#' @examples
#' rheroicons::solid$view_grid(
#'   id = 'my_view_grid_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_grid icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$view_grid <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_view_grid", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM11 13a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2v-2z"))))
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
#' @keywords rheroicons solid view_list
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/view_list.svg}
#' @examples
#' rheroicons::solid$view_list(
#'   id = 'my_view_list_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the view_list icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$view_list <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_view_list", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M3 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm0 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm0 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zm0 4a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid volume_off
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/volume_off.svg}
#' @examples
#' rheroicons::solid$volume_off(
#'   id = 'my_volume_off_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the volume_off icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$volume_off <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_volume_off", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M9.383 3.076A1 1 0 0110 4v12a1 1 0 01-1.707.707L4.586 13H2a1 1 0 01-1-1V8a1 1 0 011-1h2.586l3.707-3.707a1 1 0 011.09-.217zM12.293 7.293a1 1 0 011.414 0L15 8.586l1.293-1.293a1 1 0 111.414 1.414L16.414 10l1.293 1.293a1 1 0 01-1.414 1.414L15 11.414l-1.293 1.293a1 1 0 01-1.414-1.414L13.586 10l-1.293-1.293a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid volume_up
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/volume_up.svg}
#' @examples
#' rheroicons::solid$volume_up(
#'   id = 'my_volume_up_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the volume_up icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$volume_up <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_volume_up", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M9.383 3.076A1 1 0 0110 4v12a1 1 0 01-1.707.707L4.586 13H2a1 1 0 01-1-1V8a1 1 0 011-1h2.586l3.707-3.707a1 1 0 011.09-.217zM14.657 2.929a1 1 0 011.414 0A9.972 9.972 0 0119 10a9.972 9.972 0 01-2.929 7.071 1 1 0 01-1.414-1.414A7.971 7.971 0 0017 10c0-2.21-.894-4.208-2.343-5.657a1 1 0 010-1.414zm-2.829 2.828a1 1 0 011.415 0A5.983 5.983 0 0115 10a5.984 5.984 0 01-1.757 4.243 1 1 0 01-1.415-1.415A3.984 3.984 0 0013 10a3.983 3.983 0 00-1.172-2.828 1 1 0 010-1.415z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid x_circle
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/x_circle.svg}
#' @examples
#' rheroicons::solid$x_circle(
#'   id = 'my_x_circle_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the x_circle icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$x_circle <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_x_circle", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid x
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/x.svg}
#' @examples
#' rheroicons::solid$x(
#'   id = 'my_x_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the x icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$x <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_x", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid zoom_in
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/zoom_in.svg}
#' @examples
#' rheroicons::solid$zoom_in(
#'   id = 'my_zoom_in_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the zoom_in icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$zoom_in <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_zoom_in", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(d = "M5 8a1 1 0 011-1h1V6a1 1 0 012 0v1h1a1 1 0 110 2H9v1a1 1 0 11-2 0V9H6a1 1 0 01-1-1z")), 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8zm6-4a4 4 0 100 8 4 4 0 000-8z", 
            `clip-rule` = "evenodd"))))
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
#' @keywords rheroicons solid zoom_out
#' @references
#' \url{https://github.com/refactoringui/heroicons/blob/master/solid/zoom_out.svg}
#' @examples
#' rheroicons::solid$zoom_out(
#'   id = 'my_zoom_out_icon',
#'   class = 'my-icons',
#'   aria_hidden = FALSE,
#'   title = 'a title for the zoom_out icon'
#' )
#' @importFrom htmltools tag
#' @export
solid$zoom_out <- function(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {
    stopifnot(is.logical(aria_hidden))
    svg <- tag(`_tag_name` = "svg", list(class = "rheroicons rheroicons_solid rheroicons_zoom_out", 
        width = "20", height = "20", viewBox = "0 0 20 20", fill = "currentColor", 
        tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", d = "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z", 
            `clip-rule` = "evenodd")), tag(`_tag_name` = "path", list(`fill-rule` = "evenodd", 
            d = "M5 8a1 1 0 011-1h4a1 1 0 110 2H6a1 1 0 01-1-1z", `clip-rule` = "evenodd"))))
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

