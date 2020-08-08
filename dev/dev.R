#'////////////////////////////////////////////////////////////////////////////
#' FILE: dev.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-08-07
#' MODIFIED: 2020-08-08
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


# pkgs
usethis::use_package(package = "htmltools", min_version = TRUE)
usethis::use_package(package = "shiny")


# convert icons
system("yarn clean")
source("dev/icons_convert.R")


# checks
devtools::document()
devtools::check_man()
devtools::check()
