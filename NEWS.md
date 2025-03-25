# shinytest2 (development version)

## Breaking changes

* From a request from CRAN, creating an `AppDriver` (which uses `{chromote}`) during testing will cause the test to skip during CRAN testing. `{shinytest2}` performs this by internally calling `testthat::skip_on_cran()`.

## Bug / Improvements

* Add support for `$click()`ing `{bslib}`'s `input_task_button()` (#389).

* Improved the error message when an app takes too long to start up (@LouisLeNezet, #394).

* The `threshold` and `kernel_size` default values of the `AppDriver$expect_screenshot()` method are now configurable via two new global options: `shinytest2.compare_screenshot.threshold` and `shinytest2.compare_screenshot.kernel_size` (#401)

* `{shinytest2}` now imports `{cli}` and no longer imports `{crayon}` (@olivroy, #399).

# shinytest2 0.3.2

## Bug / Improvements

* `{shinytest2}` now uses `{rlang}` and longer depends on `{ellipsis}` (@olivroy, #382).

* `{shinytest2}` now warns rather than erroring when a potentially non-existent global variable is found in the server function, such as when column names are passed to `dplyr::select()` (thanks @matt-sd-watson @MichalLauer, #385).

# shinytest2 0.3.1

## Breaking changes

* `AppDriver$get_screenshot()`/`AppDriver$expect_screenshot()` now default an underlying `deviceScaleFactor` option to 0 instead of 1. This ensures that image sizes are more consistent across devices. To revert to prior behavior, provide `screenshot_args = list(scale = 1)` to `get_screenshot()`/`expect_screenshot()`  (#350).

## Bug / Improvements

* shinytest2 no longer checks if the computer running the tests is connected to the internet when determining if shinytest2 can connect to the server hosting the Shiny app being tested. (@Riraro #364)

# shinytest2 0.3.0

## Breaking changes

* `AppDriver$get_screenshot(selector=)`, `AppDriver$expect_screenshot(selector=)`'s default `selector` value changed from the HTML DOM selector `"html"` to `"scrollable_area"`. `"scrollable_area"` is a custom `{shinytest2}` selector that will take a screenshot of the entire scrollable area. While a breaking change, this new default value is more intuitive and robust to non-standard CSS height and width values that fail with `"html"`. (#325)

## New features

* `AppDriver$get_screenshot(selector=)`, `AppDriver$expect_screenshot(selector=)`, and their corresponding `selector` values inside `screenshot_args=`, gained two custom `{shinytest2}` values: `"scrollable_area"` and `"viewport"`. `"scrollable_area"` is the new default value and it takes a screenshot of the entire scrollable area. This is more intuitive than the previous value of the HTML DOM selector `"html"` which may result in a surprising height and width. `"viewport"` will take a screenshot of the current browser viewport. This means it will take a screenshot of whatever `$view()` is currently looking at. (#325)

* GitHub Action `rstudio/shinytest2/actions/test-app` added support for multiple directories in `app-dir`. These can be supplied using multiline string yaml syntax. See [use-ci vignette](https://rstudio.github.io/shinytest2/articles/use-ci.html#check-app-yaml-1>) for more details. (#332)

* GitHub Action `rstudio/shinytest2/actions/test-app` changed the default value of `upload-snapshots` from `false` to `true`. This is in preparation for automated snapshot handling. (#332)

## Bug / Improvements

* Set the directory to the Shiny App directory before starting the background R process. This allows for local `.Rprofile` and `.Renviron` files to be found naturally. (#275)


# shinytest2 0.2.1

* Fixed request from CRAN to correct C++11 problems in web checks (#326)

* Fixed bug where `compare_screenshot_threshold()` did not safe guard against errors thrown by `screenshot_max_difference()` (#276)

* Fixed bug where preview overflow did not have length of 1 (@cpsievert #291)

* Better support for saving global variables for a Shiny server function (#307)

* Better error message when both app.R and server.R are found (#284)

* Force JS code to be a character for `{glue}` support (#288)


# shinytest2 0.2.0

## Breaking changes

### Shiny log levels

* `AppDriver$get_logs()` has changed the `level` values when `location` equals `"shiny"`. The levels of `"error"` and `"info"` have been renamed to `"stderr"` and `"stdout"`. (#265)

### Download snapshot files

* All downloaded snapshots will contain a counter prefix (e.g. `003-` in `003-my_custom_name.txt`) to avoid having two files point to the same snapshot file location. Sharing the same snapshot file is dangerous as the last file written is stored as truth, overwriting any knowledge of the original file. (#261)
* If a `content-disposition` header is provided and `AppDriver$expect_download(name = NULL)` (default), the snapshot file will try to be saved using the `content-disposition` header value. When paired with `compare = NULL` (default), `{testthat}` will choose the proper `compare` method between `compare_file_text()` and `compare_file_binary()`. See `?testthat::expect_snapshot_file` for more details. (#261)
* `AppDriver$expect_download()` will now download snapshot files using a sanitized file name, e.g. `AppDriver$expect_download(name = "my/custom/name.txt")` will be stored in the file `tests/testthat/_snaps/003-my_custom_name.txt`. (#261)

### Timeout values

The **default** timeout values for many `AppDriver` methods have been altered and are shown in the table below. (#263)

Method | Previous value (ms) | New value (ms)
---|---:|---:
`AppDriver$new(load_timeout=)` | `10 * 1000` | `15 * 1000`
`AppDriver$set_inputs(timeout_=)` | `3 * 1000` | `4 * 1000`
`AppDriver$upload_file(timeout_=)` | `3 * 1000` | `4 * 1000`
`AppDriver$expect_js(timeout=)` | `15 * 1000` | `4 * 1000`
`AppDriver$get_js(timeout=)` | `15 * 1000` | `4 * 1000`
`AppDriver$run_js(timeout=)` | `15 * 1000` | `4 * 1000`
`AppDriver$wait_for_idle(timeout=)` | `30 * 1000` | `4 * 1000`
`AppDriver$wait_for_value(timeout=)` | `15 * 1000` | `4 * 1000`
`AppDriver$wait_for_js(timeout=)` | `30 * 1000` | `4 * 1000`
`AppDriver$stop(signal_timeout=)` | `250` | `500`

`AppDriver$new(load_timeout=)` is initialized using the first numeric value found:
1. Supplied directly: `AppDriver$new(load_timeout=)`
2. Locally defined option: `options(shinytest2.load_timeout=)`
3. System environment variable: `SHINYTEST2_LOAD_TIMEOUT`
4. `15 * 1000` (15s)

`AppDriver$new(timeout=)` is initialized using the first numeric value found:
1. Supplied directly: `AppDriver$new(timeout=)`
2. Locally defined option: `options(shinytest2.timeout=)`
3. System environment variable: `SHINYTEST2_TIMEOUT`
4. `4 * 1000` (4s)

`AppDriver$stop(signal_timeout=)` is initialized using the first numeric value found:
1. Supplied directly: `AppDriver$stop(signal_timeout=)`
2. Locally defined option: `options(shinytest2.signal_timeout=)`
3. System environment variable: `SHINYTEST2_SIGNAL_TIMEOUT`
4. If the system environment variable `R_COVR=true`: `20 * 1000` (20s)
5. `500` (0.5s)

All remaining `AppDriver` methods will default their `timeout` and `timeout_` parameters to the initialized `AppDriver$new(timeout=)` value. For example, if `app <- AppDriver$new(timeout = 500)` then `app$get_js(timeout=)` will default to `500` milliseconds.


## New Features

* `compare_screenshot_threshold()` is a new method to compare screenshots and allow small differences between two screenshots. This method checks every subset matrix of the pixel differences between the two images. If any total difference is larger than the `threshold` value, the two images are considered to be different. The subset matrix size can be increased by setting `kernel_size`. (#231)

* Support for supplying your own compare method to `AppDriver$expect_screenshot(compare=)` has been added. By default, `AppDriver$expect_screenshot(compare=)` is set to `compare_screenshot_threshold(threshold = NULL)` which is equivalent to `testthat::compare_file_binary()`. No default screenshot expectation behavior has changed. (#231)

* `test_app()` now inherits the existing test reporter when testing multiple apps within a package test file. This allows for a seamless, single reporter output instead of nested reporters being displayed. (#192)

* The recording browser window is now closed when either the "Save test and exit" or "Exit" buttons are clicked. (@daattali, #202)

* When testing and `{chromote}` can not be started, the test will be skipped. When testing on windows in CI, `{chromote}` will get an extra attempt to be started (#225)

* Make `{globals}` an `Imports` package, instead of a `Suggests` package (#223)

* Add support for _not_ recording the screen size when recording a test (#223)

## Bug / Improvements

* Fix set of bugs found by @daattali including test files should be opened in the IDE after recording and test and replace missing images in the website (#199)

* Provide example workflows on how to use `rstudio/shinytest2/actions/test-app` GHA action (#217)

* When setting a date time slider value, it can now handle array inputs properly. When recording a date time slider value, numeric values will not be recorded as milliseconds instead of seconds since epoch. (#223)

* When creating a test setup file for `{shinytest2}`, use the file path `tests/testthat/setup-shinytest2.R` instead of `tests/testthat/setup.R` to provide some quick context within the file name (#224)

* Remove trailing comma causing render bug in recorder app (@mehrnoushmalek, #239)

* Update {lintr} lints to v3 (#240)

* Fixed GHA links for `rstudio/shinytest2/actions/test-app@v1` (#249)

* Fix documentation on on `AppDriver`'s `delay` parameter being in seconds, not *milli*seconds (#255)

* Escape JS text supplied to `AppDriver$wait_for_js(script=)` (#258)

* Added support for `AppDriver$stop(signal_timeout=)`. The default timeout when sending `SIGINT`, `SIGTERM`, and `SIGKILL` signals to the `{shiny}` process is now 500ms. However, if `{covr}` is running, the default timeout is 20,000 ms to allow time for the report to generate. (#259, #263)

* Add example and FAQ entry on how to test bookmarking (#262)

* Added a DOI badge to README.md (#267)


# shinytest2 0.1.1

* Update docs for CRAN (#253)


# shinytest2 0.1.0

* Initial release of package
