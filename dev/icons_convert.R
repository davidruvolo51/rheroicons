#'////////////////////////////////////////////////////////////////////////////
#' FILE: icons_convert.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2020-08-10
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

# init base file
init_file()

# run loop
reps <- NROW(paths$outline)
pb <- txtProgressBar(max = reps)
for (i in seq_len(reps)) {

    # msg
    # cat("Converting icon", i, "of", reps, "...")

    # Generate Shiny Tag Strings from Raw SVG files
    outline <- as_svg_string(
        path = paths$outline[i, "path"],
        icon = paths$outline[i, "icon"],
        type = paths$outline[i, "type"]
    )
    solid <- as_svg_string(
        path = paths$solid[i, "path"],
        icon = paths$solid[i, "icon"],
        type = paths$solid[i, "type"]
    )

    # Generate R function
    r_code <- svg_to_rcode(
        name = paths$outline[i, "icon"],
        icons = list(
            outline = outline,
            solid = solid
        )
    )

    # write function to file
    write(
        x = r_code,
        # file = paste0("R/", paths$outline[i, "icon"], ".R"),
        file = "R/icons.R",
        append = TRUE,
        sep = "\n\n\n"
    )

    # message
    setTxtProgressBar(pb, value = i)
    # cat("complete!\n")
}