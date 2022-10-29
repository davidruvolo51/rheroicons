#' ////////////////////////////////////////////////////////////////////////////
#' FILE: test-find-icon.R
#' AUTHOR: David Ruvolo
#' CREATED: 2021-01-27
#' MODIFIED: 2022-10-14
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
        object = find_icons(query = "chevron-double"),
        expected = c(
            "chevron-double-down",
            "chevron-double-left",
            "chevron-double-right",
            "chevron-double-up"
        )
    )
})