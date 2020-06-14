#'////////////////////////////////////////////////////////////////////////////
#' FILE: icons_0_fetch.R
#' AUTHOR: David Ruvolo
#' CREATED: 2020-06-14
#' MODIFIED: 2020-06-14
#' PURPOSE: fetch source repo
#' STATUS: in.progress
#' PACKAGES: NA
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

# check dirs
if (!dir.exists("src")) dir.create("src")
# if (!dir.exists("src/heroicons")) system("rm -rf src/heroicons")

# clone
system("cd src/ && git clone https://github.com/refactoringui/heroicons/")

#' remove extra dirs
system("cd src/heroicons && rm -rf .github react scripts src vue *.yaml *.lock")