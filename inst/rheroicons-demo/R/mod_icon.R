#'////////////////////////////////////////////////////////////////////////////
#' FILE: mod_icon.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-28
#' MODIFIED: 2020-08-10
#' PURPOSE: module for icon lists and items
#' STATUS: working
#' PACKAGES: NA
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' ~ 1 ~
#' icon_ui
#' ui component for <li> element; to be used in following component
#' @param id element id
#' @param set icon family (i.e., outline or solid)
#' @param icon html markup for the icon as a string
icon_list_item_ui <- function(id, set, icon) {
    ns <- NS(id)
    tags$li(
        id = ns("icon"),
        class = "icon-item",
        tags$button(
            id = ns("icon-copy"),
            class = "icon-name icon-button",
            `data-value` = paste0(
                "rheroicons::icons$",
                id,
                "(type = \"", set, "\")"
            ),
            HTML(icon),
            tags$span(id)
        )
    )
}


#' ~ 2 ~
#' icon_list_ui
#' render <ul> containing all icons in a rheroicons set
icon_list_ui <- function(id, set) {
    ns <- NS(id)

    # define elements
    parent <- tags$ul(id = ns("icon-set"), class = "icon-list")
    children <- list()

    # outline icons
    if (set == "outline") {
        lapply(seq_len(length(rheroicons::icons)), function(d) {

            # run nth function
            html <- as.character(icons[[d]](type = "outline"))

            # render grandchild element and send to child element
            children[[d]] <<- icon_list_item_ui(
                id = names(icons[d]),
                set = "outline",
                icon = html
            )
        })
    }

    # outline icons
    if (set == "solid") {
        lapply(seq_len(length(rheroicons::icons)), function(d) {

            # run nth function
            html <- as.character(icons[[d]](type = "solid"))

            # render grandchild element and send to child element
            children[[d]] <<- icon_list_item_ui(
                id = names(icons[d]),
                set = "solid",
                icon = html
            )
        })
    }

    # bind children and return
    parent$children <- children
    return(parent)
}