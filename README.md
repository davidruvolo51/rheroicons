<!-- badges: start -->
  [![Build Status](https://travis-ci.com/davidruvolo51/rheroicons.svg?branch=prod)](https://travis-ci.com/davidruvolo51/rheroicons)
  [![R build status](https://github.com/davidruvolo51/rheroicons/workflows/R-CMD-check/badge.svg)](https://github.com/davidruvolo51/rheroicons/actions)
  <!-- badges: end -->

# rheroicons

The `rheroicons` provides access to the fantastic SVG icon collection [heroicons](https://github.com/refactoringui/heroicons), developed by [adamwathan](https://github.com/adamwathan) and [sschoger](https://github.com/sschoger), as R functions for use in your R-based web projects (i.e., shiny, Rmarkdown, etc.). This package requires no HTML, CSS, or JavaScript dependencies as all SVG icons are rendered inline.

## Install

```r
devtools::install_github("davidruvolo51/rheroicons")
```

Use the rheroicons gallery to view all of the icons included in this package: [davidruvolo.shinyapps.io/rheroicons-demo/](https://davidruvolo.shinyapps.io/rheroicons-demo/). Alternatively, you can run the gallery locally.

```r
rheroicons::launch_gallery()
```

## Use

There are well over a hundred icons in the collection and each icon has two styles (outline and solid). All icons are available in the object `icons`. 

```r
# rheroicons version of `document-add`
rheroicons::icons$document_add()
```

\***NOTE** All icons that have a dash (`-`) in the original name were renamed using underscores (`_`).

### Arguments

All functions take the following arguments.

- `type`: select the icon style (either `outline` or `solid`; default `outline`)
- `id`: a unique ID to be applied to the SVG icon
- `class`: a CSS class to be applied to the SVG icon
- `aria_hidden`: should the icon be readable or hidden from screen readers (default `FALSE`)
- `title`: add a title that describes the purpose for using the icon (ideal when `aria_hidden` is `FALSE`)

*Note: The attribute `aria_hidden` is ideal for purely aesthetic icons*

```r
library(shiny)
tags$button(
    id = "copy",
    class = "btn",
    tags$span("Add to clipboard"),
    rheroicons::icons$clipboard_copy(
        id = "copy-btn-icon",
        class = "button-icons",
        type = "solid", 
        aria_hidden = TRUE
    )
)
```

### Customizing the appearance of icons

Use the arguments `id` and `class` to define a custom identifier that can be used to select in CSS or JavaScript.

```r
rheroicons::icons$clipboard_copy(type = "outline", id = "copy-icon", class = "my-icon-set")
```

However, you may find it easier to use the predefined classes generated by this package. All icons have three types of CSS classes.

- global: `rheroicons`
- set: `rheroicons_solid` or `rheroicons_outline`
- icon: `rhericons_*`, where `*` is the name of the icon. In `rheroicons`, this is same as the function name.

The following table displays the CSS clases by set for the `arrow_circle_down` icon.

set     | rheroicons function     | rheroicons css classes
:------ | :------------------ | :---------
outline | `icons$arrow_circle_down(type = "outline")` | `rheroicons rheroicons_outline rheroicons_arrow_circle_down`
solid   | `icons$arrow_circle_down(type = "solid")` | `rheroicons rheroicons_solid rheroicons_arrow_circle_down`

In the Shiny UI, you can select items and style icons through CSS. Create a new `tags$style` element and define your styles (or use an external CSS file).

```r
# ui
ui <- tagList(
  tags$head(
    tags$style(

      # select all instances of rheroicons
      ".rheroicons {
          width: 50px;
          height: 50px;
      }",

      # select all outline icons (set color via the stroke property)
      ".rheroicons_outline {
          stroke: green;
      }",

      # select solid icons (set color via the fill property)
      ".rheroicons_solid {
          fill: red;
      }",

      # select specific icons
      ".rheroicons_home {
          width: 75px;
          height: 75px;
      }",

      # select specific icon of a particular style
      ".rheroicons_outline.rheroicons_home {
          stroke: yellow;
      }"
    )
  )
  # define UI here
  # ...
)
```

### Example

The following code demonstrates how to generate icons in Shiny, render solid and outlined icons, and style icons using CSS. 

```r
# pkgs
library(shiny)
library(rheroicons)

# ui
ui <- tagList(
    tags$head(
        tags$style(
            ".rheroicons {
                width: 50px;
                height: 50px;
            }",
            ".rheroicons_arrow_circle_down,
            .rheroicons_arrow_circle_up {
                stroke: blue;
            }",
            ".rheroicons_solid {
                fill: red;
            }"
        )
    ),
    tags$main(
        tags$h2("rheroicons"),
        tags$div(
            icons$arrow_circle_down(type = "outline"),
            icons$arrow_circle_up(type = "outline"),
            icons$arrow_circle_left(type = "outline"),
            icons$arrow_circle_right(type = "outline")
        ),
        tags$div(
            icons$chart_bar(type = "solid"),
            icons$chart_bar(type = "outline"),
        ),
        tags$div(
            icons$home(type = "solid"),
            icons$home(type = "outline"),
        )
    )
)

# server
server <- function(input, output) { }

# app
shinyApp(ui, server)
```


