#'////////////////////////////////////////////////////////////////////////////
#' FILE: _utils.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2021-01-16
#' PURPOSE: tools for converting icons into package database
#' STATUS: complete
#' PACKAGES: stringr, dplyr
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' List Files
#'
#'returns data frame containing the file path, icon type, icon name
#' build data for specific directory
#'
#' @param path a path to the icons (i.e., node_modules/...)
#'
#' @noRd
get_files <- function(path) {
    list.files(path, full.names = TRUE) %>%
        as.data.frame(., stringsAsFactors = FALSE) %>%
        mutate(
            src = stringr::str_replace(., "node_modules/heroicons/", "") %>%
                stringr::str_replace(., ".svg", "") %>%
                stringr::str_replace(., "-", "_"),
            icon = substring(
                text = src,
                first = regexpr(
                    pattern = "/",
                    text = src
                )[[1]][1] + 1
            ) %>% stringr::str_replace(., "-", "_"),
            type = substring(
                text = src,
                first = 1,
                last = regexpr(
                    pattern = "/",
                    text = src,
                )[[1]][1] - 1
            )
        ) %>%
        select(., type, icon, path = .)
}


#' Append CSS Classes
#'
#' Add rheroicons css classes to svg string. Several CSS classes are added to
#' each SVG icon. This includes a global class ("rheroicons"), an icon type
#' class ("rheroicons_outline" or "rheroicons_solid"), and a class based on
#' the icon's name ("rheroicons_academic_cap").
#'
#' @param svg`a character array containing the SVG markup for the icon
#' @param type the icon type, either "solid" or "outline"
#' @param icon the name of the icon
#'
#' @noRd
.append__css__classes <- function(svg, type, icon) {
    stringr::str_replace(
        string = svg,
        pattern = "<svg ",
        replacement = paste0(
            "<svg class=\"rheroicons rheroicons_",
            type,
            " rheroicons_",
            icon, "\" ",
            "aria-hidden=\"true\" "
        )
    )
}


#' Extract viewbox properties
#'
#' Find the attribute `viewbox` in svg string and extract the dimensions
#' in order to add the height and width attributes to the SVG element.
#'
#' @param svg a character array containing the SVG markup
#'
#' @noMd
.extract__viewbox__dims <- function(svg) {

    # locate extract viewbox attribute
    viewbox <- stringr::str_extract(
        string = svg,
        pattern = "viewBox=.([\\w\\s]+)."
    )

    # extract height and width values
    dims <- viewbox %>%
        gsub(pattern = "[^0-9]+", replacement = " ", x = .) %>%
        trimws(., "both") %>%
        stringr::str_split(string = ., pattern = "[[:space:]]") %>%
        `[[`(1) %>%
        # .[c(3, 4)] %>%
        as.list()

    # set names
    names(dims) <- c("min-x", "min-y", "width", "height")

    # return
    new_viewbox <- paste0(
        "viewbox=\"",
        dims$`min-x`, " ",
        dims$`min-y`, " ",
        dims$width, " ",
        dims$height, "\" ",
        "width=\"", dims$width, "\" ",
        "height=\"", dims$height, "\""
    )

    # replace in string
    stringr::str_replace(
        string = svg,
        pattern = viewbox,
        replacement = new_viewbox
    )
}

#' Prep SVG
#'
#' Load the contents of an svg file, add CSS classes, and create
#' structure for each icon
#'
#' @param path path to SVG icon
#' @param type the icon style; either "solid" or "outline"
#' @param icon the name of the icon
#'
#' @noRd
.process__svg <- function(path, type, icon) {
    readLines(path, warn = FALSE) %>%
        trimws(., which = "both") %>%
        paste0(., collapse = "") %>%
        .append__css__classes(svg = ., type = type, icon = icon) %>%
        .extract__viewbox__dims(svg = .)
}