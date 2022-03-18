Future release:
  * GHA integration
    * upload an artifact with updated snapshots
      * like fix_all_branches, it must be initiated manually, but _automated_

handle failures:
https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L127-L151
push_log stores information to a log file that is visible by cran and the testthat fail output logs


#' @seealso [testthat::snapshot_review()] and [testthat::snapshot_accept()] if
#'   you want to compare or update snapshots after testing. In most cases, the
#'   user is prompted to do these tasks interactively, but there are also times
#'   where it is useful to call these functions from the console.
# Documentation:

### Vignettes:
* Which do I need and why do I need them?
* Explain testing robustness
  * Robust to flakey; Sensitivity to external updates
    <!-- * `$expect_names(key)` - TODO- add this method; save outputs to snapshots if no key is provided
      * `$expect_appshot(items = value, screenshot = FALSE)` -->
    * `$expect_values(input, output, export)` - TODO- add this method; save outputs to snapshots
      * `$expect_appshot(items = value, screenshot = FALSE)`
    * `$expect_text()`
      * Text wont really change
    * `$expect_html()`
      * Classes could be added by external packages
    * `$expect_appshot()`
      * images are not reliable

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
    *
* Which do I need and why do I need them?
* Explain testing robustness
  * Add a details section in `AppDriver` docs to summarize this vignette
    * Have the methods link to the within page values
    * Find a way to link to these methods
    * Have all `$expect_*()` bundled. All `$get_*()` bundled. Link between paired method.
  * Robust to flakey; Sensitivity to external updates
    * Export internal values
    * `$expect_values(input, output, export)` - TODO- add this method; save outputs to snapshots
      * `$expect_appshot(items = value, screenshot = FALSE)`
    * `$expect_text()`
      * Text wont really change
      * Only recommended if you can not use `$expect_values()`
        * Add these types of notes to the standard docs.
    * `$expect_html()`
      * Classes could be added by external packages
    * `$expect_appshot()`
      * images are not reliable
* Pull over vignettes from {shinytest} where appropriate

### Docs:
* What are the conditions that I need this function?
* Priority sort the App docs methods
  * Tell docs that they are sorted for a reason
* Add some description about how often we guess people will use the method.
  * $execute_script(): [for non-shiny use]; Only if your JS requires a callback; Otherwise use $execute_script()
* Migration guide from shinytest -> shinytest2

## Release



## Notes from David:
Document: How to set chromote args before starting testing
  * Link to flags document
