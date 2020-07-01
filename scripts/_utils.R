#'////////////////////////////////////////////////////////////////////////////
#' FILE: _utils.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2020-07-01
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
      Map(function (name) {
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
       paste0("tag(", tagName, "list(", ., "))")
}

#' @name makeFunc
#' @details build and R function (inspired by alandipert's example app)
#' @importFrom stringr str_replace
#' @importFrom formatR tidy_source
makeFunc <- function(string, icon, type) {

    # process type
    types <- c("outline", "solid")
    if (!type %in% types) {
        stop("type not found. Use: ", types)
    }

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
        viewbox_string(string), ", "
    )

    # add optional arguments R code
    svg <- stringr::str_replace(string, "list\\(", optional_args) %>%
        paste0("svg <- ", .)

    # fix viewbox
    svg <- stringr::str_replace(svg, "viewbox", "viewBox")

    # build url for reference
    url <- paste0(
        "https://github.com/refactoringui/heroicons/blob/master/",
        type, "/", icon, ".svg"
    )

    # build function
    out <- paste0(
        "#' ", icon, "\n",
        "#' @name ", icon, "\n",
        "#' @param id a unique ID to be applied to the svg icon\n",
        "#' @param class a css class to be applied to the svg icon\n",
        "#' @param aria_hidden should the icon be readable by screen readers",
        " (default: false)\n",
        "#' @param title a string that describes the icon",
        "(should be used if aria_hidden is FALSE)\n",
        "#' @return Returns the svg markup for the heroicon ''", icon, "'\n",
        "#' @keywords rheroicons ", type, " ", icon, "\n",
        "#' @references\n",
        "#' \\url{", url, "}\n",
        "#' @examples\n",
        "#' rheroicons::", type, "$", icon, "(\n",
        "#'   id = 'my_", icon, "_icon',\n",
        "#'   class = 'my-icons',\n",
        "#'   aria_hidden = FALSE,\n",
        "#'   title = 'a title for the ", icon, " icon'\n",
        "#' )\n",
        "#' @importFrom htmltools tag\n",
        "#' @export\n",
        type, "$", icon,
        " <- function",
        "(id = NULL, class = NULL, aria_hidden = FALSE, title = NULL) {\n",
        "  stopifnot(is.logical(aria_hidden))\n",
        "  ", svg, "\n",
        "  if (!is.null(id)) { svg$attribs$id <- id }\n",
        "  if (!is.null(class)) {\n",
        "    svg$attribs$class <- paste0(svg$attribs$class, \" \", class)\n",
        "  }\n",
        "  if (isTRUE(aria_hidden)) {\n",
        "      svg$attribs$`aria-hidden` <- \"true\"\n",
        "  }\n",
        "  if (!is.null(title)) {\n",
        "    stopifnot(is.character(title))\n",
        "    svg$children <- tagList(\n",
        "        tag(\"title\", list(title)),\n",
        "        svg$children",
        "    )\n",
        "  }\n",
        "  return(svg)\n",
        "}\n"
    )
    return(out)
}

#' @name cleanFunc
#' @details formats R code
#' @importFrom formatR tidy_source
cleanFunc <- function(string) {
    string_formatted <- formatR::tidy_source(text = string, output = FALSE)
    return(string_formatted$text.tidy)
}

#' @name html2R
#' @details parse an svg string to R an function in a package
#' @references \url{https://github.com/alandipert/html2r/blob/master/app.R}
#' @importFrom XML htmlParse getNodeSet
html2R <- function(html, icon, type = "outline") {
   html %>%
      XML::htmlParse(.) %>%
      XML::getNodeSet("/html/body/*") %>%
      `[[`(1) %>%
      renderNode() %>%
      makeFunc(string = ., icon = icon, type = type) %>%
      cleanFunc(.)
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
init_file <- function(path, type) {
    file.create(path)
    header <- paste0(
        "#' ", toupper(type), " SVG Icons\n",
        "#' @name ", type, "\n",
        "#' @keywords rheroicons ", type, "\n",
        "#' @return ", type, " heroicons\n",
        "#' @references\n",
        "#' \\url{https://github.com/refactoringui/heroicons}\n",
        "#' \\url{https://davidruvolo.shinyapps.io/rheroicons-demo/}\n",
        "#' @examples\n",
        "#' rheroicons::", type, "$book_open()\n",
        "#' rheroicons::", type, "$book_open(id = 'myBookIcon')\n",
        "#' rheroicons::", type, "$book_open(class = 'my-icon-set')\n",
        "#' rheroicons::", type, "$book_open(aria_hidden = FALSE, title = 'read document')\n",
        "#' @importFrom htmltools tag\n",
        "#' @export\n",
        type, " <- list()\n"
    )
    writeLines(header, path)
}