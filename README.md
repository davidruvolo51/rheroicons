<!-- badges: start -->
![version](https://img.shields.io/github/package-json/v/davidruvolo51/rheroicons/prod?color=%2326709e)
[![R build status](https://github.com/davidruvolo51/rheroicons/workflows/R-CMD-check/badge.svg)](https://github.com/davidruvolo51/rheroicons/actions)
  <!-- badges: end -->

# rheroicons

The `rheroicons` provides access to the fantastic SVG icon collection [heroicons](https://github.com/refactoringui/heroicons), developed by [adamwathan](https://github.com/adamwathan) and [sschoger](https://github.com/sschoger), as R functions for use in your R-based web projects (i.e., shiny, Rmarkdown, etc.). This package requires no HTML, CSS, or JavaScript dependencies as all SVG icons are rendered inline.

## Install

Use `devtools` or `remotes` to install the `rheroicons`.

```r
devtools::install_github("davidruvolo51/rheroicons")
```


## Use

There are well over a 200 icons in the collection and each icon has two styles (outline and solid). Icons can be found via the icon gallery. Click an icon name to copy the R code used to generate that icon.

```r
rheroicons::launch_gallery()
```

Icons are created using the `rheroicon` function.

```r
# rheroicons version of `document-add`
rheroicons::rheroicon(name = "document_add")
```

\***NOTE**: You can use the [heroicons](https://heroicons.com) site to find icons. All icons that have a dash (`-`) in the original name were renamed using underscores (`_`).

### Arguments

All functions take the following arguments.

- `name`: an icon name
- `type`: select the icon style (either `outline` or `solid`; default `outline`)
- `classnames`: a CSS class to be applied to the SVG icon

```r
library(shiny)
tags$button(
    id = "copy",
    class = "btn",
    tags$span("Add to clipboard"),
    rheroicons::rheroicon(
        name = "clipboard_copy",
        type = "solid", 
        classnames = "btn__icons"
    )
)
```

\***NOTE**: the default icon type is `outline`


### Customizing the appearance of icons

Use the argument `class` to define a custom css classes that can be used to select icons via CSS or JavaScript.

```r
# clipboard_copy icon
rheroicons::rheroicon(
    name = "clipboard_copy",
    type = "outline", 
    class = "my__ui__icons"
)
```

However, you may find it easier to use the predefined classes generated by this package. All icons have three types of CSS classes.

- global: `rheroicons`
- set: `rheroicons_solid` or `rheroicons_outline`
- icon: `rhericons_*`, where `*` is the name of the icon. In `rheroicons`, this is same as the function name.

The following table displays the CSS clases by set for the `arrow_circle_down` icon.

set     | rheroicons function     | rheroicons css classes
:------ | :------------------ | :---------
outline | `rheroicon(name ="arrow_circle_down", type = "outline")` | `rheroicons rheroicons_outline rheroicons_arrow_circle_down`
solid   | `rheroicon(name ="arrow_circle_down", type = "solid")` | `rheroicons rheroicons_solid rheroicons_arrow_circle_down`

In the Shiny UI, you can select items and style icons through CSS. Create a new `tags$style` element and define your styles (or use an external CSS file).

```r
library(shiny)

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
            rheroicon(name = "arrow_circle_down"),
            rheroicon(name = "arrow_circle_up"),
            rheroicon(name = "arrow_circle_left"),
            rheroicon(name = "arrow_circle_right")
        ),
        tags$div(
            rheroicon(name = "chart_bar", type = "solid"),
            rheroicon(name = "chart_bar")
        ),
        tags$div(
            rheroicon(name = "home", type = "solid"),
            rheroicon(name = "home")
        )
    )
)

# server
server <- function(input, output) { }

# app
shinyApp(ui, server)
```


