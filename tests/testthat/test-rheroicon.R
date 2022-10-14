#' ////////////////////////////////////////////////////////////////////////////
#' FILE: test-rheroicon.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-09-06
#' MODIFIED: 2022-10-14
#' PURPOSE: tests for the function `rheroicon`
#' STATUS: working; ongoing
#' PACKAGES: usethis; rheroicons;
#' COMMENTS: NA
#' ////////////////////////////////////////////////////////////////////////////

#' Icon is Null
#'
#' Function fails correctly if no name is provided
test_that("function properly errors out", {
  expect_error(object = rheroicon())
})

#' IconType fails
#'
#' If an invalid icon type is provided, function should
#' fail as expected. Pass a value other than `solid` or `outline`
test_that("invalid type properly returns error", {
  expect_warning(
    object = rheroicon(name = "academic-cap", type = "circle")
  )
})

#' Class evaluation
#'
#' Does the default output of the `rheroicon` function return an HTML
#' element? The SVG icons are stored in an internal dataset as SVG
#' character strings. When used in web-based project (i.e., shiny),
#' the output should have the class `HTML`
test_that("rheroicon class", {
  expect_equal(
    object = class(rheroicon(name = "academic-cap"))[1],
    expected = "html",
    label = "Output is not an HTML element"
  )
})

#' Icon Type
#'
#' If the argument `type` is used, is the appropriate icon returned?
#' There are two styles to choose from (solid and outline). Each icon
#' has a slightly different markup and can be evaluated using the
#' class attribute.
test_that("correct icon types are returned", {
  expect_equal(
    object = c(
        stringr::str_match(
            string = rheroicon(name = "rss", type = "outline"),
            pattern = "rheroicons-outline"
        )[1],
        stringr::str_match(
            string = rheroicon(name = "rss", type = "solid"),
            pattern = "rheroicons-solid"
        )[1],
        stringr::str_match(
            string = rheroicon(name = "rss", type = "mini"),
            pattern = "rheroicons-mini"
        )[1]
    ),
    expected = c("rheroicons-outline", "rheroicons-solid", "rheroicons-mini"),
    label = "Icon types are not properly rendered"
  )
})

#' Classnames
#'
#' If the argument `classnames` is used, are CSS classes properly
#' appended to the class attribute?
test_that("classnames", {
  expect_equal(
    object = stringr::str_extract(
      string = rheroicon(name = "map", class = "my-icons"),
      pattern = "class=.([\\w\\s-])+."
    ),
    expected = "class=\"rheroicons rheroicons-outline rheroicons-map my-icons\"",
    label = "css classnames are not properly added"
  )
})