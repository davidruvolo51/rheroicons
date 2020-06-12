#'////////////////////////////////////////////////////////////////////////////
#' FILE: icons_99_utils.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-11
#' MODIFIED: 2020-06-11
#' PURPOSE: functions for parsing svg markup into R functions
#' STATUS: complete
#' PACKAGES: XML, purrr, stringr, formatR
#' COMMENTS:
#'      These utils are used to parse SVG strings to R functions. These
#'      functions were adapted from Alan Dipert's html2r example app. Some
#'      file indentation is supported using the formatR package.
#'////////////////////////////////////////////////////////////////////////////


#'//////////////////////////////////////

#' FOR TESTING

# define node for sample icon
# node <- readLines("src/heroicons/outline/camera.svg") %>%
#     paste0(., collapse = "") %>%
#     XML::htmlParse(.) %>%
#     XML::getNodeSet("/html/body/*") %>%
#     `[[`(1)

# render node to R string
# node_string <- renderNode(node)

# render R string into R package function
# node_func <- makeFunc(node_string, "camera", "outline")

# style text
# node_styled <- cleanFunc(node_func)

# write to test
# write(node_styled, "~/Desktop/test.R", append = FALSE)

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
        paste0("r-heriocons r-", icon), "\", ",
        "\"aria_hidden` = aria_hidden\", "
        )

    # add optional arguments R code
    svg <- stringr::str_replace(string, "list\\(", optional_args) %>%
        paste0("svg <- ", .)

    # build url for reference
    url <- paste0(
        "https://github.com/refactoringui/heroicons/blob/master/",
        type, "/", icon, ".svg"
    )

    # build function
    out <- paste0(
        "#' \\code{", type, "$", icon, "}\n",
        "#' @name ", icon, "\n",
        "#' @return create a SVG icon of ", icon, "\n",
        "#' @usage ", type, "$", icon, "()\n",
        "#' @param id a unique ID to be applied to the svg icon\n",
        "#' @param class a css class to be applied to the svg icon\n",
        "#' @param aria_hidden should the icon be readable by screen readers\n",
        "#' @keywords rheroicons heroicons ", type, " ", icon, "\n",
        "#' @references\n",
        "#' \\url{", url, "}\n",
        "#' \\url{https://heroicons.dev}\n",
        "#' @export\n",
        type, "$", icon,
        " <- function (id = NULL, class = NULL, aria_hidden = FALSE) {\n",
        "  stopifnot(is.logical(aria_hidden))\n",
        "  ", svg, "\n",
        "  if (!is.null(id)) { svg$attribs$id <- id }\n",
        "  if (!is.null(class)) {\n",
        "    svg$attribs$class <- paste0(svg$attribs$class, \" \", class)\n",
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
            src = stringr::str_replace(., "src/heroicons/", "") %>%
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
        "#' @details ", type, " icons from heroicons\n",
        "#' @usage rheroicons::", type, "$icon_name()\n",
        "#' @param id a unique ID to be applied to the svg icon\n",
        "#' @param class a css class to be applied to the svg icon\n",
        "#' @param aria_hidden should the icon be readable by screen readers\n",
        "#' @references\n",
        "#' \\url{https://github.com/refactoringui/heroicons}\n",
        "#' \\url{https://heroicons.dev}\n",
        "#' @examples\n",
        "#' reheroicons::", type, "$book_open()\n",
        "#' reheroicons::", type, "$book_open(id = 'myBookIcon')\n",
        "#' reheroicons::", type, "$book_open(class = 'my-icon-set')\n",
        "#' @importFrom htmltools tag\n",
        "#' @export\n",
        type, " <- list()\n"
    )
    writeLines(header, path)
}