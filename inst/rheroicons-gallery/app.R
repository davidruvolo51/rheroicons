#'////////////////////////////////////////////////////////////////////////////
#' FILE: app.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-09-09
#' MODIFIED: 2020-09-09
#' PURPOSE: app for running rheroicons gallery on shinyapps.io
#' STATUS: working
#' PACKAGES: rheroicons; shiny
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

# load and run app
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
rheroicons::launch_gallery()