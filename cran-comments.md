# CRAN Comments

## Resubmission

This is a resubmission. In this version, I have:

- revised the package description so that the phrase 'this package' is no longer used
- revised documentation so that all references to tools are wrapped in single quotes rather than double quotes
- added missing `value` tags for the function `launch_gallery` and revised existing value tags to clearly document what each function returns
- changed `\dontRun` to `if (interactive())` in the documentation for `launch_gallery`

## Test environments

- local OS X install, R 4.0.3
- windows-latest release (on GitHub Actions), R 4.0.3
- macOS-latest release (on GitHub Actions), R 4.0.3
- ubuntu-20.04 release (on GitHub Actions), R 4.0.3
- ubuntu-20.04 devel (on GitHub Actions), R 4.0.3
- win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note
