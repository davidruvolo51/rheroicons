#' Icon gallery
#'
#' Run the gallery to view and search for icons. Click on an icon
#' name to copy the 'R' code to the clipboard.
#'
#' @param ... additional 'shiny' options
#'
#' @examples
#' if (interactive()) {
#'   rheroicons::launch_gallery()
#' }
#'
#' @return A 'shiny' app containing the icon gallery
#'
#' @export
launch_gallery <- function(...) {
    shiny::shinyApp(
        ui = .ui,
        server = function(input, output, session) {
            outline_gallery <- .gallery__list("outline-icons", "outline")
            solid_gallery <- .gallery__list("solid-icons", "solid")

            shiny::observe({
                shiny::req(input$iconSet)

                if (input$iconSet == "outline") {
                    output$icons <- shiny::renderUI({
                        outline_gallery
                    })
                }
                if (input$iconSet == "solid") {
                    output$icons <- shiny::renderUI({
                        solid_gallery
                    })
                }
            })
        },
        ...
    )
}

#' gallery icon list element
#'
#' Create individual clickable component
#'
#' @param id a unique ID for the component
#' @param set icon style
#' @param icon name of the icon
#'
#' @noRd
.gallery__list__element <- function(id, set, icon) {
    ns <- shiny::NS(id)
    shiny::tags$li(
        id = ns("icon"),
        class = paste0("icon-item set-", set),
        shiny::tags$button(
            id = ns("icon-copy"),
            class = "icon-name icon-button",
            `data-value` = paste0(
                "rheroicons::rheroicon(",
                "name = \"", id, "\", ",
                "type = \"", set, "\"",
                ")"
            ),
            `data-icon` = id,
            htmltools::HTML(icon),
            shiny::tags$span(id)
        )
    )
}


#' Icon Gallery List
#'
#' render component containing all icons in a rheroicons set
#'
#' @param id a unique ID for the ui component
#' @param set icon set
#'
#' @noRd
.gallery__list <- function(id, set) {
    ns <- shiny::NS(id)

    # define elements
    parent <- shiny::tags$ul(
        id = ns("icon-set"),
        class = paste0("icon-list set-", set)
    )
    children <- list()

    # outline icons
    if (set == "outline") {
        lapply(seq_len(length(rheroicons)), function(d) {

            # run nth function
            html <- as.character(rheroicons[[d]]$icons$outline)

            # render grandchild element and send to child element
            children[[d]] <<- .gallery__list__element(
                id = names(rheroicons[d]),
                set = "outline",
                icon = html
            )
        })
    }

    # outline icons
    if (set == "solid") {
        lapply(seq_len(length(rheroicons)), function(d) {

            # run nth function
            html <- as.character(rheroicons[[d]]$icons$solid)

            # render grandchild element and send to child element
            children[[d]] <<- .gallery__list__element(
                id = names(rheroicons[d]),
                set = "solid",
                icon = html
            )
        })
    }

    # bind children and return
    parent$children <- children
    return(parent)
}

#' rheroicons gallery UI
#'
#' UI for rheroicons galler shiny app
#'
#' @noRd
.ui <- function() {

    shiny::addResourcePath(
        prefix = "rheroicons",
        directoryPath = system.file(
            "gallery-assets/public",
            package = "rheroicons"
        )
    )

    shiny::tagList(
        shiny::tags$head(
            shiny::tags$meta(charset = "utf-8"),
            shiny::tags$meta(
                `http-quiv` = "x-ua-compatible",
                content = "ie=edge"
            ),
            shiny::tags$meta(
                name = "viewport",
                content = "width=device-width, initial-scale=1"
            ),
            shiny::tags$link(
                rel = "stylesheet",
                href = "rheroicons/rheroicons.min.css"
            ),
            shiny::tags$title("rheroicons")
        ),
        shiny::tags$nav(
            id = "nav",
            class = "navbar",
            shiny::tags$ul(
                class = "menu nav-links",
                shiny::tags$li(
                    class = "menu-item",
                    shiny::tags$p(
                        class = "menu-link",
                        rheroicon(name = "photograph", type = "outline"),
                        "rheroicons"
                    )
                ),
                shiny::tags$li(
                    class = "menu-item",
                    shiny::tags$a(
                        class = "menu-link",
                        href = "https://github.com/davidruvolo51/rheroicons",
                        shiny::tag(
                            "svg",
                            list(
                                height = "30",
                                width = "30",
                                role = "img",
                                viewBox = "0 0 24 24",
                                xmlns = "http://www.w3.org/2000/svg",
                                shiny::tag(
                                    "path",
                                    list(
                                        d = "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        shiny::tags$main(
            id = "main",
            class = "main",
            shiny::tags$header(
                class = "header",
                shiny::tags$h1(
                    rheroicon(name = "photograph", type = "outline"),
                    "rheroicons"
                ),
                shiny::tags$p(
                    "The", shiny::tags$strong("rheroicons"), "package",
                    "brings the fantastic SVG",
                    "icon collection",
                    shiny::tags$a(
                        href = "https://github.com/refactoringui/heroicons",
                        "heroicons"
                    ),
                    "to R for use in your R-based web projects."
                ),
                shiny::tags$cite(
                    "by",
                    shiny::tags$a(
                        href = "https://github.com/davidruvolo51",
                        "@dcruvolo"
                    )
                ),
            ),
            shiny::tags$section(
                class = "section",
                shiny::tags$h2("Available Icons"),
                shiny::tags$p(
                    "View outline or solid icons. Click an icon to copy the",
                    "R code to the clipboard."
                ),
                shiny::tags$form(
                    class = "form",
                    shiny::tags$legend("Options"),
                    shiny::tags$fieldset(
                        id = "iconSet",
                        class = "select-input-group hidden",
                        `data-group` = "iconSet",
                        shiny::tags$label(
                            `for` = "select_icon_family",
                            "Select an Icon Set"
                        ),
                        shiny::tags$button(
                            id = "icon-set",
                            class = "select-input-parent",
                            `data-group` = "iconSet",
                            shiny::tags$span(
                                class = "select-input-selected",
                                "-- Choose --"
                            ),
                            rheroicon(
                                name = "chevron_down",
                                type = "outline",
                                class = "select-input-parent-icon"
                            )
                        ),
                        shiny::tags$ol(
                            id = "icon-set-options",
                            class = "select-input-options hidden",
                            `data-group` = "iconSet",
                            shiny::tags$li(
                                class = "select-input-option",
                                `data-group` = "iconSet",
                                shiny::tags$button(
                                    id = "set-option-a",
                                    class = "select-input-option-button",
                                    `data-group` = "iconSet",
                                    `data-value` = "outline",
                                    rheroicon(
                                        name = "check_circle",
                                        type = "solid",
                                        class = "selected-icon"
                                    ),
                                    "Outline"
                                )
                            ),
                            shiny::tags$li(
                                class = "select-input-option",
                                `data-group` = "iconSet",
                                shiny::tags$button(
                                    id = "set-option-b",
                                    class = "select-input-option-button",
                                    `data-group` = "iconSet",
                                    `data-value` = "solid",
                                    rheroicon(
                                        name = "check_circle",
                                        type = "solid",
                                        class = "selected-icon"
                                    ),
                                    "Solid"
                                )
                            )
                        )
                    )
                ),
                shiny::uiOutput("icons")
            ),
            shiny::tags$textarea(
                id = "icon-clipboard",
                class = "visually-hidden"
            )
        ),
        shiny::tags$div(
            id = "status-success",
            class = "status-box",
            rheroicon(
                name = "thumb_up",
                type = "outline"
            ),
            shiny::tags$p("Successfully copied icon!")
        ),
        shiny::tags$div(
            id = "status-failed",
            class = "status-box",
            rheroicon(
                name = "exclamation",
                type = "outline"
            ),
            shiny::tags$p("Copy Failed")
        ),
        shiny::tags$script(src = "rheroicons/rheroicons.min.js")
    )
}