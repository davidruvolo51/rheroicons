#'////////////////////////////////////////////////////////////////////////////
#' FILE: icons_convert.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2020-08-07
#' PURPOSE: convert SVG files to R functions
#' STATUS: working
#' PACKAGES: XML, purrr, stringr, formatR
#' COMMENTS: install source via npm
#'////////////////////////////////////////////////////////////////////////////

# pkgs
suppressPackageStartupMessages(library(dplyr))

# source utils
source("dev/_utils.R")

#'//////////////////////////////////////

#' ~ 1 ~
# build list of svg files

paths <- list(
    outline = get_files("node_modules/heroicons/outline"),
    solid = get_files("node_modules/heroicons/solid")
)

# validate file structure
stopifnot(
    "number of files match" = NROW(paths$outline) == NROW(paths$solid),
    "icon sets are identical match" = paths$outline$icon == paths$solid$icon
)

# 


# run loop
reps <- 1# NROW(files)
for (i in seq_len(reps)) {

    # msg
    cat("Converting icon", i, "of", reps, "...")

    # read raw svg files
    raw_outline <- readLines(paths$outline[i, "path"]) %>% paste0(., collapse = "")
    raw_solid <- readLines(paths$solid[i, "path"]) %>% paste0(., collapse = "")

    # convert functions
    svg_to_r <- html2R(icons = list(outline = raw_outline, solid = raw_solid))

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



# files <- bind_rows(
#     get_files("node_modules/heroicons/outline"),
#     get_files("node_modules/heroicons/solid")
# )

# # init primary R files
# init_file(path = "R/outline.R", type = "outline")
# init_file(path = "R/solid.R", type = "solid")