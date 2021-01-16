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
# init_file()

# run loop
rheroicons <- list()
reps <- NROW(paths$outline)
pb <- txtProgressBar(max = reps)
for (i in seq_len(reps)) {

    # build master object
    rheroicons[[paths$outline$icon[i]]] <- list(
        icons = list(
                outline = .process__svg(
                path = paths$outline$path[i],
                type = paths$outline$type[i],
                icon = paths$outline$icon[i]
            ),
            solid = .process__svg(
                path = paths$solid$path[i],
                type = paths$solid$type[i],
                icon = paths$solid$icon[i]
            )
        )
    )

    # message
    setTxtProgressBar(pb, value = i)
}
