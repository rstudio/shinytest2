
* `$expect_values()`
  * expects values only
  * `$expect_values(screenshot = self$values_screenshot)`; `AppDriver$new(values_screenshot = getOption("shinytest2.values_screenshot", TRUE))`
  * `if (values_screenshot == TRUE)`
    * saves screenshot into `_snaps` folder and "annouce"s the file to avoid deletion
      * Should create a "new" image that diffviewer can view. But should NOT throw an error when the images are different
      * Creates too many commits for unnecessary changes
        * values-002-FLAG.png
        * .gitignore `-FLAG.png`
      * Allows for "logging" prior screenshot knowledge
        * NOT an expectation
* $expect_screenshot()
  * expects screenshot only


# TODO
## Week 1
* Add `$get_values()`
  * And all other "get" methods
* wait_for_stable(duration = 500) -> wait_for_idle(duration = 500)
* Remove `wait_` from API
* Transport chromote methods to {chromote}

## Make *public* on GitHub
* Notify Eric and David

## Week 2 (pray)

* Script for migrating from {shinytest} to {shinytest2}
  * AST
    * lose comments and formatting
  * grep
    * harder in long run
  * hybrid
    * Check out wch/staticdocs, r-lib/styler (or even r-lib/lintr)
    * maybe extract comments and reinsert after AST conversion


## Week 3-4 (document)

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
* Pull over vignettes from {shinytest} where appropriate

### Docs:
* What are the conditions that I need this function?
* Priority sort the App docs methods
  * Tell docs that they are sorted for a reason
* Add some description about how often we guess people will use the method.
  * $execute_script_callback(): rare; Only if your JS requires a callback; Otherwise use $execute_script()
* Migration guide from shinytest -> shinytest2

## Release
