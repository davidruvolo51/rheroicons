## rheroicons 0.1.5

* Restructured `outline` and `solid` icon lists to `icons`. Rendering icon style is now done using the `type` argument. By default, `type = "outline"`.
* Updated gallery to reflect changes with icon styles
* Added unit testing (see `tests/testthat/`)
* Added CI tests
* removed `yarn clean` and rewrote it in `dev/dev.R` as R code

## rheroicons 0.1.4

* Added a `NEWS.md` file to track changes to the package.
* Added Package management dir `dev`. Convert Icons using `dev/dev.R`