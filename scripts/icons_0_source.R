#'////////////////////////////////////////////////////////////////////////////
#' FILE: icons_0_source.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2020-06-11
#' PURPOSE: convert SVG files to R functions
#' STATUS: in.progress
#' PACKAGES: XML, purrr, stringr, formatR
#' COMMENTS:
#'////////////////////////////////////////////////////////////////////////////

# pkgs
suppressPackageStartupMessages(library(dplyr))

# source utils
source("scripts/icons_99_utils.R")

#'//////////////////////////////////////

#' ~ 0 ~
# clone source icons
#' system("cd src/ && git clone 'https://github.com/refactoringui/heroicons/'")

#' remove extra dirs
# system("cd src/heroicons && rm -rf .github react scripts src vue *.yaml *.json *.lock *.md")

#'//////////////////////////////////////

#' ~ 1 ~
# build list of svg files
files <- bind_rows(
    get_files("src/heroicons/outline"),
    get_files("src/heroicons/solid")
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
