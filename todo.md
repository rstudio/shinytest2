make vignette of steps
make an automatic conversion function?
  keep if >90% success

talk to lionel about snapshots

issue in testthat for keeping snapshots if they are not touched
  https://github.com/r-lib/testthat/issues/1143

automate platform / R version suffix

gha
  upload an artifact with updated snapshots
    like fix_all_branches, it must be initiated manually, but _automated_


https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L99-L102
  # Announce snapshot file before touching `fig` in case evaluation
  # causes an error. This allows testthat to restore the files
  # (see r-lib/testthat#1393).
  testthat::announce_snapshot_file(name = file)


handle failures:
https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L127-L151
push_log stores information to a log file that is visible by cran and the testthat fail output logs

provide custom function to compare the files:
https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L132


use the annouce_snapshot_file w/ the `suffix` so that they other files are not deleted. Do not announce the main file?

Make two classes of shinytest objects:
* One for shinytest2 legacy that throws deprecation messages and errors.
  * ShinyDriver2Legacy$new()?
* Another that is the ideal shinytest2 object. This should not contain deprecated methods
  * ShinyDriver2$new() ?
  * Shintest2$new() ?

Alpha order input values when serializing to json

conversion notes
  shinytest -> shinytest2
  phantomTimeout -> X


  `$findElement()` does not support `linkText`, `partialLinkText`, `xpath`

  <!-- $executeScript -> $executeScript -->
  $executeScriptAsync -> $executeScriptCallback

  `Widget$new(element=)` -> `Widget2$new(nodeId=)`

  app$snapshot(*) -> expect_shinytest2_snapshot(app, *)

  make a second class for ShinyDriver2Legacy
    ShinyDriver2$initalize() Added `...`
    ShinyDriver2Legacy$snapshotInit(path=) does not makes sense
    ShinyDriver2Legacy$snapshotInit(screenshot=) was moved to ShinyDriver2$initialize(screenshot=)


ShinyDriver2$waitFor returns `FALSE` if a condition is not found in time. (Not `NA`)
execut




#' @seealso [testthat::snapshot_review()] and [testthat::snapshot_accept()] if
#'   you want to compare or update snapshots after testing. In most cases, the
#'   user is prompted to do these tasks interactively, but there are also times
#'   where it is useful to call these functions from the console.


ShinyDriver$self$waitForShiny -> ShinyDriver2$wait_for_idle
ShinyDriver$self$findWidget -> gone




* move all documentation back to within AppDriver
* rename shinydriver to AppDriver
* make test_app()

* remove all external methods such as `app_METHOD()` that are wrappers to `app$METHOD()`

list_widgets -> get_input_names, get_output_names
... -> arguments
get_all_values -> get_values


get_all_values(input, output, export) -> get_values(input, output, export)
  why not `get_input_values()`?
  What about `get_names()`
  list widgets is a JS method that looks for shiny classes,
    why not use `get_values()` and

Implement get_names() -> this should give all `input`, `output`, and `export` names via `get_values()`

Requirements for release:
  * shinytest -> shinytest2 conversion file
    * cover the good bits, throw on more custom bits
  * readme
  * website
    * vignettes
Future release:
  * GHA integration


# Documentation:

### Vignettes:
* Which do I need and why do I need them?
* Explain testing robustness
  * Robust to flakey; Sensitivity to external updates
    * `$expect_names(key)` - TODO- add this method; save outputs to snapshots if no key is provided
      * `$expect_appshot(items = value, screenshot = FALSE)`
    * `$expect_value(value)` - TODO- add this method; save outputs to snapshots
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


### TODO
  * Drop `$get_names()`, `$get_values()`
  * Rename `$wait_for_condition(expr=)` to `$wait_for_script(script=)`
  * Remove `$wait_for_value()` in favor of `$wait_for_stable()`
