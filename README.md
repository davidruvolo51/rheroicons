# rheroicons

rheroicons provides the fantastic SVG icon collection [heroicons](https://github.com/refactoringui/heroicons), developed by [adamwathan](https://github.com/adamwathan) and [sschoger](https://github.com/sschoger), as inline R functions for use in your R-based web projects (i.e., shiny, Rmarkdown, etc.). This package requires no html, css, or javascript dependencies as all SVG icons are rendered inline.

## Install

```r
devtools::install_github("davidruvolo51/rheroicons")
```

## Use

There are well over a hundred icons in total. Each icon has two styles: outline and solid. To create an icon, type `style$icon()`.

- `set` : choose either `outline` or `solid`.
- `icon` : an icon name, e.g., `puzzle`, `volume_up`, `code`, etc.

Use the heroicons site to search for icons: [https://heroicons.dev](https://heroicons.dev).

**NOTE**: Icon names with a dash `-` were rewritten with an underscore `_`.

- Source: `arrow-circle-down`
- rheroicons: `arrow_circle_down`

### Arguments

All functions take the following arguments.

- `id`: a unique ID to be applied to the svg icon
- `class`: a css class to be applied to the svg icon
- `aria_hidden`: should the icon be readable by screen readers (default: false)

*Note: The attribute `aria_hidden` is ideal for purely aesthetic icons*

```r
library(shiny)
tags$button(
    id = "copy",
    class = "btn",
    tags$span("Add to clipboard"),
    rheroicons::outline$clipboard_copy(class = "btn-icon", aria_hidden = TRUE)
)
```

### Customizing Icons

You can use the arguments `id` and `class` to select and style icons using CSS. Alternatively, you can use one or more of the predefined classes. All icons have the global class, a icon set class, and an icon name class.

- global: all icons have the class `rheroicons`
- set: either `rheroicons-solid` or `rheroicons-outline`
- icon: `rhericons-*`, where `*` is the icon name (same as the function name).  For example, `rheroicons-home` or `rheroicons-arrow_circle_down`.

### Example

The following code demonstrates how to generate icons in Shiny, render solid and outlined icons, and style icons using css. 

```r
# pkgs
library(shiny)
library(rheroicons)

# ui
ui <- tagList(
    tags$head(
        tags$style(
            ".rheroicons-arrow_circle_down,
            .rheroicons-arrow_circle_up {
                stroke: blue;
            }",
            ".rheroicons-solid {
                fill: red;
            }"
        )
    ),
    tags$main(
        tags$h2("rheroicons"),
        tags$div(
            outline$arrow_circle_down(),
            outline$arrow_circle_up(),
            outline$arrow_circle_left(),
            outline$arrow_circle_right()
        ),
        tags$div(
            solid$chart_bar(),
            outline$chart_bar(),
        ),
        tags$div(
            solid$home(),
            outline$home(),
        )
    )
)

# server
server <- function(input, output) { }

# app
shinyApp(ui, server)
```


