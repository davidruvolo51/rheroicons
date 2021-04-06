# rheroicons 0.3.2

* Upgraded to Heroicons `v1.0` (released on 29 March 2021). This brings fixes to several icons.

# rheroicons 0.3.1

This is a minor package update.

* fixed package descriptions (description, single quotes throughout)
* fixed `launch_gallery` docs (missing `\value`)
* fixed `launch_gallery` example so that it uses `if (interactive())`

# rheroicons 0.3.0

* Prepared package for CRAN submission
* Removed `cli` package as a dependency
* Added tests for `find_icon`
* Revised documentation

# rheroicons 0.2.4

* Upgraded to latest version of Heroicons ([v0.4.2](https://github.com/tailwindlabs/heroicons/releases/tag/v0.4.2))

# rheroicons 0.2.3

This is a minor package update. The main issue was the handling of error messages via the `cli` package. These changes are listed below.

* Updated error message for `rheroicon` function. It now uses {.val {value}}
* Removed error message for find_icons as the default query is "".
* Reset `pkgbump` configuration file

# rheroicons 0.2.21

* The development side of this package now uses Webpack!
* Tested assets in `dev` and `prod` environments using the `dev-app`
* Rebuilt assets
* Updated R package configuration and ignore files

# rheroicons 0.2.2

* Updated to parcel `v2.0`
* Introduced `find_icon` function

# rheroicons 0.2.1

* Restructured static assets for the rheroicons gallery and fixed resource path

# rheroicons 0.2.0

* New package structure! Icons are now generated using the function `rheroicon`. Select an icon using the argument `name`. Icons can be found in the gallery via `launch_gallery()` function. Use the argument `type` to return define the icon style as `outline` or `solid`. Icons can be further customized by passing additional CSS classes using the `classnames` argument. 
* Restructured rheroicons gallery as icons are available using an internal dataset. The gallery's client, server, and modules are now located in `R/launch_gallery.R`. Static assets are still located in `inst/rheroicons-demo`.
* Rewrote unit tests

## rheroicons 0.1.6

* Updated to Heroicons `v0.4.0`
* Redesigned icon gallery

## rheroicons 0.1.5

* Restructured `outline` and `solid` icon lists to `icons`. Rendering icon style is now done using the `type` argument. By default, `type = "outline"`.
* Updated gallery to reflect changes with icon styles
* Added unit testing (see `tests/testthat/`)
* Added CI tests
* removed `yarn clean` and rewrote it in `dev/dev.R` as R code

## rheroicons 0.1.4

* Added a `NEWS.md` file to track changes to the package.
* Added Package management directory `dev`. Convert Icons using `dev/dev.R`