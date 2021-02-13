#'////////////////////////////////////////////////////////////////////////////
#' FILE: test-launch-gallery.R
#' AUTHOR: David Ruvolo
#' CREATED: 2021-01-30
#' MODIFIED: 2021-02-13
#' PURPOSE: unit tests for launch gallery
#' STATUS: working; ongoing
#' PACKAGES: usethis; rheroicons; shiny
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' .gallery__list__element()
#' Make sure icons are returned as li element
test_that("Markup of gallery element is expected", {

    # choose a random outline icon
    res <- .gallery__list__element(
        id = "test",
        set = "outline",
        icon = rheroicons[["zoom_out"]][["icons"]][["solid"]]
    )

    expect_equal(
        object = c(
            res$name,
            res$attribs$class,
            res$children[[1]]$name,
            names(attributes(res$children[[1]]$children[[1]]))[1],
            res$children[[1]]$children[[2]]$name
        ),
        expected = c(
            "li",
            "icon-item set-outline",
            "button",
            "html", # grab attribute instead
            "span"
        ),
        label = "Gallery element does not have expected markup"
    )
})


#' gallery_list()
#' Test both sets
test_that("Icon sets render properly", {

    set_sol <- .gallery__list(id = "test", set = "solid")
    set_out <- .gallery__list(id = "test", set = "outline")

    expect_equal(
        object = c(
            set_sol$attribs$class,
            set_out$attribs$class
        ),
        expected = c(
            "icon-list set-solid",
            "icon-list set-outline"
        ),
        label = "Solid and Outline icons are marked up properly"
    )
})

#' I'm not sure if these tests are helpful, but I guess I have to
#' please the coverage overlords!

#' launch_gallery()
test_that("start gallery returns a shiny object", {
    expect_equal(
        object = class(launch_gallery(options = c(port = 1234))),
        expected = "shiny.appobj",
        label = "Start gallery does not return a shiny object"
    )
})

#' .ui()
#' This isn't a great unit test, but return and eval the primary
#' html elements
test_that("UI function returns expected markup", {

    res <- .ui()
    expect_equal(
        object = c(
            class(res),
            res[[2]]$name,
            res[[3]]$name,
            res[[4]]$name,
            res[[5]]$name,
            res[[6]]$name
        ),
        expected = c(
            "shiny.tag.list", "list",
            "nav",
            "main",
            "div",
            "div",
            "script"
        )
    )
})