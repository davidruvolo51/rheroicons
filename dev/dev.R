#'////////////////////////////////////////////////////////////////////////////
#' FILE: dev.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-08-07
#' MODIFIED: 2022-10-19
#' PURPOSE: package management
#' STATUS: ongoing
#' PACKAGES: usethis; pkgbump
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' install dev packages
#' install.packages("devtools")
#' install.packages("usethis")
#' remotes::install_github("r-lib/revdepcheck")

#' init primary files
#' usethis::use_namespace()
#' usethis::use_description()
#' usethis::use_travis()
#' usethis::use_github_action_check_standard()
#' usethis::use_news_md()
#' usethis::use_testthat()
#' usethis::use_mit_license("David Ruvolo")

#'//////////////////////////////////////

#' ~ 1 ~
#' SVG Processing

#' load conversion tools
rheroicons <- rjson::fromJSON(file="dev/prepared-data/rheroicons.2022-10-19.json")

#' update internal data object
usethis::use_data(rheroicons, internal = TRUE, overwrite = TRUE)

#'//////////////////////////////////////

#' ~ 2 ~
#' Package Management

#' ~ 2a ~
#' pkgs
usethis::use_package(package = "htmltools", min_version = TRUE)
usethis::use_package(package = "shiny")
usethis::use_package(package = "stringr")

#' ~ 2b ~
#' pkg checks
devtools::check_man()
devtools::load_all()
devtools::test()
devtools::check()

#' ~ 2c ~
#' cran checks
covr::package_coverage()
covr::report()
devtools::check(cran = TRUE, env_vars = c(NOT_CRAN = FALSE), manual = TRUE)
devtools::spell_check()
devtools::run_examples()
devtools::test()
devtools::check_win_release()
devtools::check_win_devel()

#' ~ 2c ~
#' pkgbump configuration
#' remotes::install_github("davidruvolo51/pkgbump")
pkgbump::set_pkgbump(files = c("package.json", "DESCRIPTION"))
pkgbump::pkgbump(version = "0.3.2")

#' ~ 2d ~
# ignore files
ignore <- c(
  "__pycache__",
  "downloads",
  "node_modules",
  "inst/rheroicons-gallery/rsconnect",
  "yarn-error.log"
)

usethis::use_git_ignore(ignore)
usethis::use_build_ignore(
  files = c(
    ignore,
    ".github",
    "config",
    "CRAN-SUBMISSION",
    "dev",
    "inst/rheroicons-gallery",
    ".babelrc",
    ".gitignore",
    "cran-comments.md",
    "LICENSE.md",
    "package.json",
    "pkgbump.config.json",
    "src",
    "rheroicons.code-workspace",
    "webpack.config.js",
    "pnpm-lock.yaml",
    "yarn.lock",
    "vite.config.js"
  )
)

#' ~ 2e ~
#' Release / Submit Package
#' Run the following code only if all checks have passed. This includes
#' all checks in 2B and 2C, as well as GitHub Actions. Push to GitHub
#' after `check_win_*` tests have been run and you have received the
#' confirmation email.
devtools::release()