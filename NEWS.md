# rheroicons 0.2.0

* New package structure :rocket: icons are now generated using the function `rheroicon`. Select an icon using the argument `name`. Icons can be found in the gallery via `launch_gallery()` function. Use the argument `type` to return define the icon style as `outline` or `solid`. Icons can be further customized by passing additional css classes using the `classnames` argument. 
* Restructured rheroicons gallery as icons are available using an internal dataset. The gallery's ui, server, and modules are now located in `R/launch_gallery.R`. Static assets are still located in `inst/rheroicons-demo`.
* Rewrote unit tests

## rheroicons 0.1.6

* Updated to heroicons `v0.4.0`
* Redesigned icon gallery

## rheroicons 0.1.5

* Restructured `outline` and `solid` icon lists to `icons`. Rendering icon style is now done using the `type` argument. By default, `type = "outline"`.
* Updated gallery to reflect changes with icon styles
* Added unit testing (see `tests/testthat/`)
* Added CI tests
* removed `yarn clean` and rewrote it in `dev/dev.R` as R code

## rheroicons 0.1.4

* Added a `NEWS.md` file to track changes to the package.
* Added Package management dir `dev`. Convert Icons using `dev/dev.R`