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


√ ShinyDriver$self$waitForShiny -> ShinyDriver2$wait_for_idle
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
  * √ Fix recorder
    * √ unknown fn
    * √ path - remove
    * √ test file name: test-NAME.R
    * √ spacing after library call
  * Expectations:
    * Have all expect methods return the `x` value used in the `expect_equal(x, y)`
    * Add `expect_values(input = missing_arg(), output = missing_arg(), export = missing_arg(), ..., cran = TRUE)`, and maybe `expect_input()`, `expect_output()`, `expect_export()`
    * Add `expect_screenshot()`
      * make `variant` a `missing_arg()` and have it throw on `expect_screenshot()` if not supplied.
    * Remove `expect_appshot()`
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


### TODO
  * Drop `$get_names()`, `$get_values()`
  * Rename `$wait_for_condition(expr=)` to `$wait_for_script(script=)`
  * Remove `$wait_for_value()` in favor of `$wait_for_stable()`




execute_script_callback should not exist
  they can write a promise and await promise

add getter methods for all expectations
add $get_variant()

## TODO; addd
app$expect_screenshot(selector) # assert screenshot equals
app$take_screenshot(selector) # screenshot helper file
app$get_screenshot(selector, file) # save to file

app$expect_screenshot() # document that this is just equal to the next line
expect_snapshot_file(app$get_screenshot(), variant = app$get_variant(), compare = testthat::compare_file_binary)

app$expect_values()
expect_snapshot_value(app$get_values(), variant = app$get_variant(), compare = testthat::compare_file_text)




app <- AppDriver$new(appDir)

app$click(id = "foo")
app$set_inputs(foo = "click")

app$click(id = "foo")
app$click(id = "foo2")
app$set_inputs(foo = "click", Z = "click")


app$uploadFile(myFileInput = "filePath")
app$click("Z")

app$set_inputs(Z = "click")

app$set_inputs(myFileInput = "filePath", Z = "click")





app$expect_snapshot() # `shinytest`

app$expect_appshot()
  app$take_screenshot(selector) # screenshot helper file
  app$expect_values()

app$expect_values()
# json values

app$expect_screenshot(selector)
app$take_screenshot(selector)
# screenshot value

## TODO; addd
app$expect_screenshot(selector) # assert screenshot equals
app$take_screenshot(selector) # screenshot helper file
app$get_screenshot(selector, file) # save to file





app$expect_appshot(id = "winston")
  app$expect_values(auto = "winston")
  app$expect_screenshot(selector = find_container("winston"))













----------------------------------------------




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
* √ `$expect_values(screenshot = self$values_screenshot)` and `$expect_screenshot()` only. No appshot
* √ Add `$get_values()`
  * √ And all other "get" methods
* √ wait_for_stable(duration = 500) -> wait_for_idle(duration = 500)
* Make `variant` off by default. Must opt-in for `$expect_screenshot()` (which can be `NULL`)
  * √ Add `$get_variant()`
* Remove `wait_` from `{shinytest2}` API
* Transport `chromote_eval()` to `{chromote}`

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
