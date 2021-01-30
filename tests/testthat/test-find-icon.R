#' ////////////////////////////////////////////////////////////////////////////
#' FILE: test-find-icon.R
#' AUTHOR: David Ruvolo
#' CREATED: 2021-01-27
#' MODIFIED: 2021-01-27
#' PURPOSE: `find_icon` tests
#' STATUS: working
#' PACKAGES: testthat
#' COMMENTS: NA
#' ////////////////////////////////////////////////////////////////////////////


#' ~ 1 ~
#' Function returns entire list of icons if no query is provided
test_that("entire icon list prints correctly", {
    expect_equal(
        object = length(find_icons()),
        expected = length(rheroicons),
        label = "Returned array does not match the length of the icon set"
    )
})

#' ~ 2 ~
#' Query returns expected icons
test_that("query returns expected icons", {
    expect_equal(
        object = find_icons(query = "chevron_double"),
        expected = c(
            "chevron_double_down",
            "chevron_double_left",
            "chevron_double_right",
            "chevron_double_up"
        )
    )
})