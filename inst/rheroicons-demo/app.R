#'////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-24
#' MODIFIED: 2020-08-25
#' PURPOSE: rheroicons demo
#' STATUS: working
#' PACKAGES: shiny; rheroicons
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

# install
# devtools::install_github("davidruvolo51/rheroicons@dev")
# remove.packages("rheroicons")

# pkgs
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(rheroicons))

# pre-render icon UIs
outline_ui <- icon_list_ui("outline-icons", "outline")
solid_ui <- icon_list_ui("solid-icons", "solid")

# ui
ui <- tagList(
    singleton(
        tags$head(
            tags$meta(charset = "utf-8"),
            tags$meta(`http-quiv` = "x-ua-compatible", content = "ie=edge"),
            tags$meta(
                name = "viewport",
                content = "width=device-width, initial-scale=1"
            ),
            tags$link(
                rel = "stylesheet",
                href = "rheroicons.min.css"
            ),
            tags$title("rheroicons")
        )
    ),
    tags$nav(
        id = "nav",
        class = "navbar",
        tags$ul(
            class = "menu nav-links",
            tags$li(
                class = "menu-item",
                tags$p(
                    class = "menu-link",
                    rheroicons::icons$photograph(type = "outline"),
                    "rheroicons"
                )
            ),
            tags$li(
                class = "menu-item",
                tags$a(
                    class = "menu-link",
                    href = "https://github.com/davidruvolo51/rheroicons",
                    tag(
                        "svg",
                        list(
                            height = "30",
                            width = "30",
                            role = "img",
                            viewBox = "0 0 24 24",
                            xmlns = "http://www.w3.org/2000/svg",
                            tag(
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
    tags$main(
        id = "main",
        class = "main",
        tags$header(
            class = "header",
            tags$h1(
                rheroicons::icons$photograph(type = "outline"),
                "rheroicons"
            ),
            tags$p(
                "The", tags$strong("rheroicons"), "package",
                "brings the fantastic SVG",
                "icon collection",
                tags$a(
                    href = "https://github.com/refactoringui/heroicons",
                    "heroicons"
                ),
                "to R for use in your R-based web projects."
            ),
            tags$cite(
                "by",
                tags$a(
                    href = "https://github.com/davidruvolo51",
                    "@dcruvolo"
                )
            ),
        ),
        tags$section(
            class = "section",
            tags$h2("Available Icons"),
            tags$p(
                "View outline or solid icons. Click an icon to copy the",
                "R code to the clipboard."
            ),
            tags$form(
                class = "form",
                tags$legend("Options"),
                tags$fieldset(
                    id = "iconSet",
                    class = "select-input-group hidden",
                    `data-group` = "iconSet",
                    tags$label(
                        `for` = "select_icon_family",
                        "Select an Icon Set"
                    ),
                    tags$button(
                        id = "icon-set",
                        class = "select-input-parent",
                        `data-group` = "iconSet",
                        tags$span(
                            class = "select-input-selected",
                            "-- Choose --"
                        ),
                        icons$chevron_down(
                            type = "outline",
                            aria_hidden = TRUE,
                            class = "select-input-parent-icon"
                        )
                    ),
                    tags$ol(
                        id = "icon-set-options",
                        class = "select-input-options hidden",
                        `data-group` = "iconSet",
                        tags$li(
                            class = "select-input-option",
                            `data-group` = "iconSet",
                            tags$button(
                                id = "set-option-a",
                                class = "select-input-option-button",
                                `data-group` = "iconSet",
                                `data-value` = "outline",
                                icons$check_circle(
                                    type = "solid",
                                    aria_hidden = TRUE,
                                    class = "selected-icon"
                                ),
                                "Outline"
                            )
                        ),
                        tags$li(
                            class = "select-input-option",
                            `data-group` = "iconSet",
                            tags$button(
                                id = "set-option-b",
                                class = "select-input-option-button",
                                `data-group` = "iconSet",
                                `data-value` = "solid",
                                icons$check_circle(
                                    type = "solid",
                                    aria_hidden = TRUE,
                                    class = "selected-icon"
                                ),
                                "Solid"
                            )
                        )
                    )
                )
            ),
            uiOutput("icons")
        ),
        tags$textarea(
            id = "icon-clipboard",
            class = "visually-hidden"
        )
    ),
    tags$div(
        id = "status-success",
        class = "status-box",
        icons$thumb_up(type = "outline", aria_hidden = TRUE),
        tags$p("Successfully copied icon!")
    ),
    tags$div(
        id = "status-failed",
        class = "status-box",
        icons$exclamation(type = "outline", aria_hidden = FALSE),
        tags$p("Copy Failed")
    ),
    tags$script(src = "rheroicons.min.js")
)

# server
server <- function(input, output, session) {

    observe({
        if (input$iconSet == "outline") {
            output$icons <- renderUI({
                outline_ui
            })
        }
        if (input$iconSet == "solid") {
            output$icons <- renderUI({
                solid_ui
            })
        }
    })
}


# app
shinyApp(ui, server)