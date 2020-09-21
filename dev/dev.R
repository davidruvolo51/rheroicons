#'////////////////////////////////////////////////////////////////////////////
#' FILE: dev.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-08-07
#' MODIFIED: 2020-09-21
#' PURPOSE: package management
#' STATUS: ongoing
#' PACKAGES: usethis; pkgbump
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
usethis::use_package(package = "stringr")
usethis::use_package(package = "cli")

# convert icons
source("dev/convert.R")

# use dataset
usethis::use_data(rheroicons, internal = TRUE, overwrite = TRUE)

# checks
devtools::check_man()
devtools::load_all()
devtools::test()
devtools::check()

# init pkgbump
pkgbump::set_pkgbump(
    files = c(
        "package.json",
        "DESCRIPTION"
    )
)

pkgbump::pkgbump(version = "0.2.2")