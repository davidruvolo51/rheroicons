#'////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-09-09
#' MODIFIED: 2020-09-09
#' PURPOSE: dev app
#' STATUS: ongoing
#' PACKAGES: rheroicons; etc.
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

devtools::load_all()
# launch_gallery(options = list(port = 9000, launch.browser = TRUE))

# #" find icons tests
# find_icons(query = "circle")
# find_icons(query = "(\\_down)$")

# library(shiny)

# # ui
# ui <- tagList(
#   tags$head(
#     tags$style(
#       "body {
#         font-size: 16pt;
#         background-color: #f6f6f6;
#         font-family: Helvetica, Arial, sans-serif;
#         margin: 0;
#         padding: 0;
#       }",
#       "main {
#         margin: 0 auto;
#         box-sizing: content-box;
#         padding: 2em;
#         max-width: 972px;
#       }",
#       ".rheroicons {
#         width: 75px;
#         height: 75px;
#       }",
#       ".rheroicons-solid {
#         fill: #2C497F; 
#       }",
#       ".circle-icons {
#         stroke: #417B5A;
#       }",
#       ".rheroicons-home-modern.rheroicons-outline {
#         width: 44px;
#         height: 44px;
#       }"
#     )
#   ),
#   tags$main(
#     tags$h2("My Shiny App"),
#     tags$p("This is my cool Shiny application. Check out some of the icons"),
#     tags$div(
#       rheroicons::rheroicon(name = "arrow-down-circle", class="circle-icons"),
#       rheroicons::rheroicon(name = "arrow-up-circle", class="circle-icons"),
#       rheroicons::rheroicon(name = "arrow-left-circle", class="circle-icons"),
#       rheroicons::rheroicon(name = "arrow-right-circle", class="circle-icons")
#     ),
#     tags$div(
#       rheroicons::rheroicon(name = "musical-note", type = "solid"),
#       rheroicons::rheroicon(name = "musical-note")
#     ),
#     tags$div(
#       rheroicons::rheroicon(name = "home-modern", type = "solid"),
#       rheroicons::rheroicon(name = "home-modern")
#     )
#   )
# )

# # server
# server <- function(input, output) { }

# # app
# shinyApp(ui, server)