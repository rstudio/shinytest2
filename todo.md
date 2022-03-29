Future release:
  * GHA integration
    * upload an artifact with updated snapshots
      * like fix_all_branches, it must be initiated manually, but _automated_


#' @seealso [testthat::snapshot_review()] and [testthat::snapshot_accept()] if
#'   you want to compare or update snapshots after testing. In most cases, the
#'   user is prompted to do these tasks interactively, but there are also times
#'   where it is useful to call these functions from the console.

# Documentation:

### Docs:
* What are the conditions that I need this function?
* Priority sort the App docs methods
  * Tell docs that they are sorted for a reason
* Add some description about how often we guess people will use the method.
  * $execute_script_callback(): rare; Only if your JS requires a callback; Otherwise use $execute_script()
* Migration guide from shinytest -> shinytest2

### Vignettes:
* Getting started
  * intro intro intro
    * walk through the bare minimum to get started using shinytest2
    * Copy source of geyser
    * Record test
    * run test
  * Medium difficulty example:
    * Use an `$expect_screenshot()` with a `variant`
    * explain what a variant is
  * How to review different variants

### Docs:
* What are the conditions that I need this function?
* Priority sort the App docs methods
  * Tell docs that they are sorted for a reason
* Fix vignette links
* Link Robust vignette from AppDriver docs
* Fix markdown table in https://rstudio.github.io/shinytest2/articles/ci.html#renv-lock-renvactivate-r--rprofile
* Add some description about how often we guess people will use the method.
  * $execute_script(): [for non-shiny use]; Only if your JS requires a callback; Otherwise use $execute_script()
* Migration guide from shinytest -> shinytest2

## Release



## Notes from David:
Document: How to set chromote args before starting testing
  * Link to flags document
