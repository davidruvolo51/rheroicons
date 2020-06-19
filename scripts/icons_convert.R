#'////////////////////////////////////////////////////////////////////////////
#' FILE: icons_convert.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2020-06-19
#' PURPOSE: convert SVG files to R functions
#' STATUS: in.progress
#' PACKAGES: XML, purrr, stringr, formatR
#' COMMENTS: install source via npm
#'////////////////////////////////////////////////////////////////////////////

# pkgs
suppressPackageStartupMessages(library(dplyr))

# source utils
source("scripts/_utils.R")

#'//////////////////////////////////////

#' ~ 1 ~
# build list of svg files
files <- bind_rows(
    get_files("node_modules/heroicons/outline"),
    get_files("node_modules/heroicons/solid")
)

# init primary R files
init_file(path = "R/outline.R", type = "outline")
init_file(path = "R/solid.R", type = "solid")

# run loop
reps <- NROW(files)
for (i in seq_len(reps)) {

    # parsing file
    cat("Converting file", i, "of", reps, "...")

    # read and convert svg to R
    raw_svg <- readLines(files[i, "path"]) %>% paste0(., collapse = "")
    svg_to_r <- html2R(
        html = raw_svg,
        icon = files[i, "icon"],
        type = files[i, "type"]
    )

    # write function to file
    write(
        x = svg_to_r,
        file = paste0("R/", files[i, "type"], ".R"),
        append = TRUE,
        sep = "\n\n\n"
    )

    # message
    cat("complete!\n")
}
