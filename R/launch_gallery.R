#' \code{launch_gallery}
#'
#' This function starts the rheroicons gallery
#' @return This function starts the rheroicons gallery
#' @usage rheroicons::launch_gallery()
#' @importFrom shiny runApp
#' @keywords rheroicons launch_gallery demo
#' @export
launch_gallery <- function() {
    path <- system.file("rheroicons-demo", package = "rheroicons")
    if (path == "") stop("Demo app does not exist.", call. = FALSE)
    runApp(path, display.mode = "normal")
}