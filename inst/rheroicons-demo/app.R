#'////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-24
#' MODIFIED: 2020-06-29
#' PURPOSE: rheroicons demo
#' STATUS: in.progress
#' PACKAGES: shiny; rheroicons
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

# install
# devtools::install_github("davidruvolo51/rheroicons@prod")

# pkgs
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(rheroicons))

# source
source("modules/mod_icon.R")

# pre-render icon UIs
outlineUI <- icon_list_ui("outline-icons", "outline")
solidUI <- icon_list_ui("solid-icons", "solid")

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
                    "rheroicons",
                    outline$sparkles(aria_hidden = TRUE),
                )
            )
        )
    ),
    tags$main(
        id = "main",
        class = "main",
        tags$section(
            class = "section",
            tags$h1("rheroicons"),
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
            tags$form(
                class = "form",
                tags$fieldset(
                    id = "iconSet",
                    class = "select-input-group",
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
                        rheroicons::outline$chevron_down(
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
                                rheroicons::solid$check_circle(
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
                                rheroicons::solid$check_circle(
                                    aria_hidden = TRUE,
                                    class = "selected-icon"
                                ),
                                "Solid"
                            )
                        )
                    )
                )
            )
        ),
        tags$section(
            class = "icon-section",
            tags$h2("Available Icons"),
            tags$p("Click an icon to copy the name to the clipboard."),
            uiOutput("icons"),
        ),
        tags$textarea(
            id = "icon-clipboard",
            class = "visually-hidden"
        )
    ),
    tags$div(
        id = "status-success",
        class = "status-box",
        rheroicons::outline$thumb_up(aria_hidden = TRUE),
        tags$p("Successfully copied icon!")
    ),
    tags$div(
        id = "status-failed",
        class = "status-box",
        rheroicons::outline$exclamation(aria_hidden = FALSE),
        tags$p("Copy Failed")
    ),
    tags$script(src = "rheroicons.min.js")
)

# server
server <- function(input, output, session) {

    observe({
        if (input$iconSet == "outline") {
            output$icons <- renderUI({
                outlineUI
            })
        }
        if (input$iconSet == "solid") {
            output$icons <- renderUI({
                solidUI
            })
        }
    })
}


# app
shinyApp(ui, server)