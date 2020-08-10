#'////////////////////////////////////////////////////////////////////////////
#' FILE: test-rheroicons.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-08-10
#' MODIFIED: 2020-08-10
#' PURPOSE: icon tests
#' STATUS: in.progress
#' PACKAGES: rheroicons; testthat
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' ~ 1 ~
#' Evaluate Base Markup
#' Make sure function returns a proper SVG element
test_that("base markup assessment", {
    el <- icons$adjustments()
    expect_equal(
        object = el$name,
        expected = "svg",
        label = "Primary output is not an SVG element"
    )
})


#' ~ 2 ~
#' Evaluate Icon Style
#' confirm that argument type returns the proper icon
test_that("evaluation of argument type", {
    styles <- c(
        icons$adjustments(type = "outline")$attribs$`data-icon-set`,
        icons$adjustments(type = "solid")$attribs$`data-icon-set`
    )
    expect_equal(
        object = styles,
        expected = c("outline", "solid"),
        label = "Argument 'type' does not return proper icon"
    )
})


#' ~ 3 ~
#' ID argument
#' confirm that the ID attribute is added to the svg element
test_that("evaluation of argument 'id'", {
    el <- icons$adjustments(id = "adjustIcon")
    expect_equal(
        object = length(el$attribs$id),
        expected = 1,
        label = "Argument 'id' does not return expected attribute"
    )
})


#' ~ 4 ~
#' Class argument
#' confirm that user defined css classes are added to existing classes
test_that("evaluation of argument 'class'", {
    el <- strsplit(
        x = icons$adjustments(class = "my_icon_set")$attribs$class,
        split = " "
    )[[1]]
    expect_equal(
        object = el[length(el)],
        expected = "my_icon_set",
        label = "Argument 'class' does not return user defined class"
    )
})


#' ~ 4 ~
#' ARIA Hidden Attribute
#' confirm that aria hidden attribute is properly set
test_that("evaluation of argument 'aria_hidden'", {
    el <- c(
        length(icons$adjustments(aria_hidden = FALSE)$attribs$`aria-hidden`),
        length(icons$adjustments(aria_hidden = TRUE)$attribs$`aria-hidden`)
    )
    expect_equal(
        object = el,
        expected = c(0, 1),
        label = "Argument 'aria_hidden' does not return expected value"
    )
})



#' ~ 5 ~
#' Title Argument
#' confirm that a <title> element is added to the markup
#' (this element should be the first child)
test_that("evaluation of argument 'title'", {
    el <- icons$adjustments(title = "Make adjustments")
    expect_equal(
        object = el$children[[1]]$name,
        expected = "title",
        label = "'title' element does not render as expected"
    )
})