#'////////////////////////////////////////////////////////////////////////////
#' FILE: dev.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-08-07
#' MODIFIED: 2020-08-10
#' PURPOSE: package management
#' STATUS: ongoing
#' PACKAGES: usethis;
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////


# init primary files
usethis::use_namespace()
usethis::use_description()
usethis::use_travis()
usethis::use_github_action_check_standard()
usethis::use_news_md()
usethis::use_testthat()


# pkgs
usethis::use_package(package = "htmltools", min_version = TRUE)
usethis::use_package(package = "shiny")

# clean up existing icons
sapply(
    list.files("R", full.names = TRUE)[list.files("R") != "launch_gallery.R"],
    file.remove
)

# convert icons
source("dev/icons_convert.R")

# unit testing
devtools::test()
#' devtools::load_all()


# checks
devtools::check_man()
devtools::check()

