#'////////////////////////////////////////////////////////////////////////////
#' FILE: _utils.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2020-08-10
#' PURPOSE: functions for parsing svg markup into R functions
#' STATUS: complete
#' PACKAGES: XML, purrr, stringr, formatR, dplyr
#' COMMENTS:
#'      These utils are used to parse SVG strings to R functions. These
#'      functions were adapted from Alan Dipert's html2r example app. Some
#'      file indentation is supported using the formatR package.
#'////////////////////////////////////////////////////////////////////////////

#' FOR TESTING

# define node for sample icon
# library(dplyr)
# node <- readLines("node_modules/heroicons/outline/ban.svg") %>%
#     paste0(., collapse = "") %>%
#     XML::htmlParse(.) %>%
#     XML::getNodeSet("/html/body/*") %>%
#     `[[`(1)
# node_string <- renderNode(node)
# node_func <- makeFunc(node_string, "camera", "outline")
# node_styled <- cleanFunc(node_func)
# write(node_styled, "~/Desktop/test.R", append = FALSE)

#'//////////////////////////////////////

# define a function that extracts viewBox values and converts it to R string
viewbox_string <- function(string) {
    l <- stringr::str_extract(string, '"viewbox" = "(.*?)"') %>%
        gsub(pattern = "[^0-9]+", replacement = " ", x = .) %>%
        trimws(., "both") %>%
        stringr::str_split(string = ., pattern = "[[:space:]]") %>%
        `[[`(1) %>%
        .[c(3, 4)] %>%
        as.list()
    names(l) <- c("width", "height")
    paste0("\"", names(l), "\" = \"", l, "\"", collapse = ", ")
}

#'//////////////////////////////////////


#' @name makeAttrs
#' @details
#'      Modified from the original function "makeAttrs", this function
#'      extracts and reformats svg attributes for use in R.
#' @references \url{https://github.com/alandipert/html2r/blob/master/app.R}
#' @importFrom XML xmlAttrs
makeAttrs <- function(node) {
   attrs <- XML::xmlAttrs(node)
   names(attrs) %>%
      Map(function(name) {
         val <- attrs[[name]]
         paste0(
            '"', name, '"',
            " = ",
            # process value
            if (val == "") "NA" else paste0('"', val, '"')
        )
      }, .)
}


#' @name Keep
#' @references \url{https://github.com/alandipert/html2r/blob/master/app.R}
Keep <- function(fun, xs) Map(fun, xs) %>% Filter(Negate(is.null), .)


#' @name renderNode
#' @details renders svg elements into R code
#' @importFrom XML xmlName xmlChildren
#' @importFrom purrr partial
#' @references \url{https://github.com/alandipert/html2r/blob/master/app.R}
renderNode <- function(node, indent = 0) {
    tagName <- paste0("`_tag_name` = ", "\"", XML::xmlName(node), "\", ")
    newIndent <- indent + length(tagName) + 1
    XML::xmlChildren(node) %>%
       Keep(purrr::partial(renderNode, indent = newIndent), .) %>%
       append(makeAttrs(node), .) %>%
       paste(collapse = ", ") %>%
       trimws(which = c("left")) %>%
       paste0("htmltools::tag(", tagName, "list(", ., "))")
}

#' Render SVG
#'
#' Render node set into SVG icon
#'
#' @param node output from render node
#' @param icon name of the icon
#' @param type icon style type (i.e., outline or solid)
#'
#' @noRd
renderSVG <- function(node, icon, type) {

    # string to inject input arguments into <svg> element
    optional_args <- paste0(
        "list\\(",
        "\"class\" = \"",
        paste0(
            "rheroicons", # global class
            " rheroicons_", type, # icon style class
            " rheroicons_", icon  # icon class
        ),
        "\", ",
        viewbox_string(node), ", ",
        "`data-icon-set` = \"", type, "\", "
    )

    # add optional arguments R code + fix viewBox
    svg <- stringr::str_replace(node, "list\\(", optional_args) %>%
        stringr::str_replace(., pattern = "viewbox", replacement = "viewBox")

    return(svg)

}


#' SVG to R Code
#'
#' Wrap two SVG shiny tag strings into an R function
#'
#' @param name name of the icone
#' @param icons a list containing svg string for solid and outline icon
#'
#' @importFrom stringr str_replace
#' @importFrom formatR tidy_source
#'
#' @noRd
svg_to_rcode <- function(name, icons) {

    # build url for reference
    urls <- list(
        outline = paste0(
            "https://github.com/refactoringui/heroicons/blob/master/",
            "outline/", name, ".svg"
        ),
        solid = paste0(
            "https://github.com/refactoringui/heroicons/blob/master/",
            "solid/", name, ".svg"
        )
    )

    # build function
    out <- paste0(
        "#' ", name, "\n",
        "#' @name ", name, "\n",
        "#' @description Render an svg icon of a ", name, "\n",
        "#' @param type render an outline or solid icon (default outline)\n",
        "#' @param id a unique ID to be applied to the svg icon\n",
        "#' @param class a css class to be applied to the svg icon\n",
        "#' @param aria_hidden should the icon be hidden from screen readers",
        " (default: TRUE)\n",
        "#' @param title a string that describes the icon (optional)\n",
        "#' @examples\n",
        "#' rheroicons::icons$", name, "(\n",
        "#'   type = \"solid\",\n",
        "#'   id = \"my_", name, "_icon\",\n",
        "#'   class = \"my-icons\",\n",
        "#'   aria_hidden = FALSE,\n",
        "#'   title = \"a title for the ", name, " icon\"\n",
        "#' )\n",
        "#' @return Render an svg icon of a ", name, "\n",
        "#' @keywords rheroicons ", name, "\n",
        # "#'\n",
        # "#' @export\n",
        "icons$",
        name,
        " <- function",
        "(type = \"outline\", id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {\n",
        "  stopifnot(\n",
        "    \"Aria Hidden must be TRUE or FALSE\" = is.logical(aria_hidden),\n",
        "    \"Invalid icon type\" = type %in% c(\"solid\", \"outline\")\n",
        "  )\n",
        "  icon_styles <- list(\n",
        "    outline = ", icons$outline, ",\n",
        "    solid = ", icons$solid, "\n",
        "  )\n",
        "  el <- icon_styles[[type]]\n",
        "  if (!is.null(id)) {\n",
        "    el$attribs$id <- id\n",
        "  }\n",
        "  if (!is.null(class)) {\n",
        "    el$attribs$class <- paste0(el$attribs$class, \" \", class)\n",
        "  }\n",
        "  if (isTRUE(aria_hidden)) {\n",
        "      el$attribs$`aria-hidden` <- \"true\"\n",
        "  }\n",
        "  if (!is.null(title)) {\n",
        "    stopifnot(is.character(title))\n",
        "    el$children <- htmltools::tagList(\n",
        "        htmltools::tag(\"title\", list(title)),\n",
        "        el$children\n",
        "    )\n",
        "  }\n",
        "  return(el)\n",
        "}\n"
    )
    return(out)
}


#' As SVG String
#'
#' Convert Raw SVG to R Shiny Tag String from file
#'
#' @param path file path to SVG file
#' @param icon name of the icon
#' @param type icon style (outline or solid)
#'
#' @references \url{https://github.com/alandipert/html2r/blob/master/app.R}
#' @importFrom XML htmlParse getNodeSet
#'
#' @noRd
as_svg_string <- function(path, icon, type) {
    readLines(path) %>%
    paste0(., collapse = "") %>%
    XML::htmlParse(.) %>%
    XML::getNodeSet("/html/body/*") %>%
    `[[`(1) %>%
    renderNode(.) %>%
    renderSVG(node = ., icon = icon, type = type) %>%
    cleanFunc(.)
}


#' @name cleanFunc
#' @details formats R code
#' @importFrom formatR tidy_source
cleanFunc <- function(string) {
    string_formatted <- formatR::tidy_source(text = string, output = FALSE)
    return(string_formatted$text.tidy)
}


#' @name get_files
#' @details returns data frame containing the file path, icon type, icon name
# build data for specific directory
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


# @name init_files
# @details a short function that creates the output files with notes
init_file <- function() {
    file.create("R/icons.R")
    header <- paste0(
        "#' Heroicons for R\n",
        "#'\n",
        "#' @references\n",
        "#' \\url{https://github.com/refactoringui/heroicons}\n",
        "#' \\url{https://davidruvolo.shinyapps.io/rheroicons-demo/}\n",
        "#'\n",
        "#' @examples\n",
        "#' rheroicons::icons$book_open()\n",
        "#' rheroicons::icons$book_open(type = \"outline\")\n",
        "#' rheroicons::icons$book_open(type = \"solid\")\n",
        "#' rheroicons::icons$book_open(id = \"myBookIcon\")\n",
        "#' rheroicons::icons$book_open(class = \"my-icon-set\")\n",
        "#' rheroicons::icons$book_open(aria_hidden = FALSE)\n",
        "#' rheroicons::icons$book_open(title = \"read documentation\")\n",
        "#'\n",
        "#' @keywords rheroicons\n",
        "#' @export\n",
        "icons <- list()\n"
    )
    writeLines(header, "R/icons.R")
}