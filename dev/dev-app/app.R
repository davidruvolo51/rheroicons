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
launch_gallery(options = list(port = 9000, launch.browser = TRUE))