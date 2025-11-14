# Drive a Shiny application

This class starts a Shiny app in a new R session, along with chromote's
headless browser that can be used to simulate user actions. This
provides a full simulation of a Shiny app so that you can test user
interactions with a live app.

Methods described below are ordered by perceived popularity. *Expect*
methods are grouped next to their corresponding *get* methods.

## Vignettes

Please see [Testing in
depth](https://rstudio.github.io/shinytest2/articles/in-depth.html) for
more details about the different expectation methods.

Please see [Robust
testing](https://rstudio.github.io/shinytest2/articles/robust.html) for
more details about the cost / benefits for each expectation method.

## Test mode

To have your `AppDriver` retrieve values from your Shiny app, be sure to
set `shiny::runApp(test.mode = TRUE)` when running your Shiny app.

If you are deploying your Shiny app where you do not have control over
the call to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), you can
set `options(shiny.testmode = TRUE)` in a `.Rprofile` file within your
Shiny app directory.

## Start-up failure

If the app throws an error during initialization, the `AppDriver` will
be stored in `rlang::last_error()$app`. This allows for the "failure to
initialize" to be signaled while also allowing for the `app` to be
retrieved after any initialization error has been thrown.

## Exporting reactive values

Reactive values from within your Shiny application can be exported using
the method:
[[`shiny::exportTestValues()`](https://rdrr.io/pkg/shiny/man/exportTestValues.html)](https://shiny.posit.co/r/reference/shiny/latest/exporttestvalues.html).
This underutilized method exposes internal values of your app without
needing to create a corresponding input value or output value.

For example:

    library(shiny)
    shiny_app <- shinyApp(
      fluidPage(
        h1("Pythagorean theorem"),
        numericInput("A", "A", 3),
        numericInput("B", "B", 4),
        verbatimTextOutput("C"),
      ),
      function(input, output) {
        a_squared <- reactive({ req(input$A); input$A * input$A })
        b_squared <- reactive({ req(input$B); input$B * input$B })
        c_squared <- reactive({ a_squared() + b_squared() })
        c_value <- reactive({ sqrt(c_squared()) })
        output$C <- renderText({ c_value() })

        exportTestValues(
          a_squared = { a_squared() },
          b_squared = { b_squared() },
          c_squared = { c_squared() }
        )
      }
    )

    app <- AppDriver$new(shiny_app)

    init_vals <- app$get_values()
    str(init_vals)
    #> List of 3
    #> $ input :List of 2
    #> ..$ A: int 3
    #> ..$ B: int 4
    #> $ output:List of 1
    #> ..$ C: chr "5"
    #> $ export:List of 3
    #> ..$ a_squared: int 9
    #> ..$ b_squared: int 16
    #> ..$ c_squared: int 25

These exported test values are only exposed when
`shiny::runApp(test.mode = TRUE)` is set. shinytest2 sets this variable
when running Shiny based app or document.

## testthat wrappers

The two main expectation methods: `$expect_values()` and
`$expect_screenshot()` eventually wrap
[`testthat::expect_snapshot_file()`](https://testthat.r-lib.org/reference/expect_snapshot_file.html).

Their underlying logic is similar to:

    skip_on_cran() # AppDriver

    ## Expect values
    tmpfile <- tempfile(fileext = ".json")
    jsonlite::write_json(app$get_values(), tmpfile)
    expect_snapshot_file(
      tmpfile,
      variant = app$get_variant(),
      compare = testthat::compare_file_text
    )


    ## Expect screenshot
    tmpfile <- tempfile(fileext = ".png")
    app$get_screenshot(tmpfile)
    expect_snapshot_file(
      tmpfile,
      variant = app$get_variant(),
      compare = testthat::compare_file_binary
    )

To update the snapshot values, you will need to run a variation of
[`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html).

## See also

[`platform_variant()`](https://rstudio.github.io/shinytest2/reference/platform_variant.md),
[`use_shinytest2_test()`](https://rstudio.github.io/shinytest2/reference/use_shinytest2.md)

## Methods

### Public methods

- [`AppDriver$new()`](#method-AppDriver-new)

- [`AppDriver$view()`](#method-AppDriver-view)

- [`AppDriver$click()`](#method-AppDriver-click)

- [`AppDriver$set_inputs()`](#method-AppDriver-set_inputs)

- [`AppDriver$upload_file()`](#method-AppDriver-upload_file)

- [`AppDriver$expect_values()`](#method-AppDriver-expect_values)

- [`AppDriver$get_value()`](#method-AppDriver-get_value)

- [`AppDriver$get_values()`](#method-AppDriver-get_values)

- [`AppDriver$expect_download()`](#method-AppDriver-expect_download)

- [`AppDriver$get_download()`](#method-AppDriver-get_download)

- [`AppDriver$expect_text()`](#method-AppDriver-expect_text)

- [`AppDriver$get_text()`](#method-AppDriver-get_text)

- [`AppDriver$expect_html()`](#method-AppDriver-expect_html)

- [`AppDriver$get_html()`](#method-AppDriver-get_html)

- [`AppDriver$expect_js()`](#method-AppDriver-expect_js)

- [`AppDriver$get_js()`](#method-AppDriver-get_js)

- [`AppDriver$run_js()`](#method-AppDriver-run_js)

- [`AppDriver$expect_screenshot()`](#method-AppDriver-expect_screenshot)

- [`AppDriver$get_screenshot()`](#method-AppDriver-get_screenshot)

- [`AppDriver$wait_for_idle()`](#method-AppDriver-wait_for_idle)

- [`AppDriver$wait_for_value()`](#method-AppDriver-wait_for_value)

- [`AppDriver$wait_for_js()`](#method-AppDriver-wait_for_js)

- [`AppDriver$expect_unique_names()`](#method-AppDriver-expect_unique_names)

- [`AppDriver$get_dir()`](#method-AppDriver-get_dir)

- [`AppDriver$get_url()`](#method-AppDriver-get_url)

- [`AppDriver$get_window_size()`](#method-AppDriver-get_window_size)

- [`AppDriver$set_window_size()`](#method-AppDriver-set_window_size)

- [`AppDriver$get_chromote_session()`](#method-AppDriver-get_chromote_session)

- [`AppDriver$get_variant()`](#method-AppDriver-get_variant)

- [`AppDriver$get_logs()`](#method-AppDriver-get_logs)

- [`AppDriver$log_message()`](#method-AppDriver-log_message)

- [`AppDriver$stop()`](#method-AppDriver-stop)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize an `AppDriver` object

#### Usage

    AppDriver$new(
      app_dir = testthat::test_path("../../"),
      ...,
      name = NULL,
      variant = missing_arg(),
      seed = NULL,
      load_timeout = missing_arg(),
      timeout = missing_arg(),
      wait = TRUE,
      screenshot_args = missing_arg(),
      expect_values_screenshot_args = TRUE,
      check_names = TRUE,
      view = missing_arg(),
      height = NULL,
      width = NULL,
      clean_logs = TRUE,
      shiny_args = list(),
      render_args = NULL,
      options = list()
    )

#### Arguments

- `app_dir`:

  This value can be many different things:

  - A directory containing your Shiny application or a run-time Shiny R
    Markdown document.

  - A URL pointing to your shiny application. (Don't forget to set
    `testmode = TRUE` when running your application!)

  - A Shiny application object which inherits from `"shiny.appobj"`.

  By default, `app_dir` is set to `test_path("../../")` to work in both
  interactive and testing usage.

  If a file path is not provided to `app_dir`, snapshots will be saved
  as if the root of the Shiny application was the current directory.

- `...`:

  Must be empty. Allows for parameter expansion.

- `name`:

  Prefix value to use when saving testthat snapshot files. Ex:
  `NAME-001.json`. Name **must** be unique when saving multiple
  snapshots from within the same testing file. Otherwise, two different
  `AppDriver` objects will be referencing the same files.

- `variant`:

  If not-`NULL`, results will be saved in `_snaps/{variant}/{test.md}`,
  so `variant` must be a single string of alphanumeric characters
  suitable for use as a directory name.

  You can variants to deal with cases where the snapshot output varies
  and you want to capture and test the variations. Common use cases
  include variations for operating system, R version, or version of key
  dependency. For example usage, see
  [`platform_variant()`](https://rstudio.github.io/shinytest2/reference/platform_variant.md).

- `seed`:

  An optional random seed to use before starting the application. For
  apps that use R's random number generator, this can make their
  behavior repeatable.

- `load_timeout`:

  How long to wait for the app to load, in ms. This includes the time to
  start R. Defaults to 15s.

  If `load_timeout` is missing, the first numeric value found will be
  used:

  - R option `options(shinytest2.load_timeout=)`

  - System environment variable `SHINYTEST2_LOAD_TIMEOUT`

  - `15 * 1000` (15 seconds)

- `timeout`:

  Default number of milliseconds for any `timeout` or `timeout_`
  parameter in the `AppDriver` class. Defaults to 4s.

  If `timeout` is missing, the first numeric value found will be used:

  - R option `options(shinytest2.timeout=)`

  - System environment variable `SHINYTEST2_TIMEOUT`

  - `4 * 1000` (4 seconds)

- `wait`:

  If `TRUE`, `$wait_for_idle(duration = 200, timeout = load_timeout)`
  will be called once the app has connected a new session, blocking
  until the Shiny app is idle for 200ms.

- `screenshot_args`:

  Default set of arguments to pass in to
  [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)'s
  `$get_screenshot()` method when taking screenshots within
  `$expect_screenshot()`. To disable screenshots by default, set to
  `FALSE`.

- `expect_values_screenshot_args`:

  The value for `screenshot_args` when producing a debug screenshot for
  `$expect_values()`. To disable debug screenshots by default, set to
  `FALSE`.

- `check_names`:

  Check if widget names are unique once the application initially loads?
  If duplicate names are found on initialization, a warning will be
  displayed.

- `view`:

  Opens the
  [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)
  in an interactive browser tab before attempting to navigate to the
  Shiny app.

- `height, width`:

  Window size to use when opening the
  [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html).
  Both `height` and `width` values must be non-null values to be used.

- `clean_logs`:

  Whether to remove the `stdout` and `stderr` Shiny app logs when the
  `AppDriver` object is garbage collected.

- `shiny_args`:

  A list of options to pass to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). Ex:
  `list(port = 8080)`.

- `render_args`:

  Passed to `rmarkdown::run(render_args=)` for interactive `.Rmd`s. Ex:
  `list(quiet = TRUE)`

- `options`:

  A list of [`base::options()`](https://rdrr.io/r/base/options.html) to
  set in the Shiny application's child R process. See
  [`shiny::shinyOptions()`](https://rdrr.io/pkg/shiny/man/shinyOptions.html)
  for inspiration. If `shiny.trace = TRUE`, then all WebSocket traffic
  will be captured by `chromote` and time-stamped for logging purposes.

#### Returns

An object with class `AppDriver` and the many methods described in this
documentation.

#### Examples

    \dontrun{
    # Create an AppDriver from the Shiny app in the current directory
    app <- AppDriver()

    # Create an AppDriver object from a different Shiny app directory
    example_app <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver(example_app)

    # Expect consistent inital values
    app$expect_values()
    }

------------------------------------------------------------------------

### Method `view()`

View the Shiny application

Calls `$view()` on the
[`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)
object to *view* your Shiny application in a Chrome browser.

This method is very helpful for debugging while writing your tests.

#### Usage

    AppDriver$view()

#### Examples

    \dontrun{
    # Open app in Chrome
    app$view()
    }

------------------------------------------------------------------------

### Method `click()`

Click an element

Find a Shiny input/output value or DOM CSS selector and click it using
the [DOM method
`TAG.click()`](https://www.w3schools.com/jsref/met_html_click.asp).

This method can be used to click input buttons and other elements that
need to simulate a click action.

#### Usage

    AppDriver$click(
      input = missing_arg(),
      output = missing_arg(),
      selector = missing_arg(),
      ...
    )

#### Arguments

- `input, output, selector`:

  A name of an Shiny `input`/`output` value or a DOM CSS selector. Only
  one of these may be used.

- `...`:

  If `input` is used, all extra arguments are passed to
  `$set_inputs(!!input := "click", ...)`. This means that the
  `AppDriver` will wait until an output has been updated within the
  specified `timeout_`. When clicking any other content, `...` must be
  empty.

#### Examples

    \dontrun{
    app_path <- system.file("examples/07_widgets", package = "shiny")
    app <- AppDriver$new(app_path)

    tmpfile <- write.csv(cars, "cars.csv")
    app$upload_file(file1 = tmpfile)
    cat(app$get_text("#view"))
    app$set_inputs(dataset = "cars", obs = 6)
    app$click("update")
    cat(app$get_text("#view"))
    }

------------------------------------------------------------------------

### Method `set_inputs()`

Set input values

Set Shiny inputs by sending the value to the Chrome browser and
programmatically updating the values. Given `wait_ = TRUE`, the method
will not return until an output value has been updated.

#### Usage

    AppDriver$set_inputs(
      ...,
      wait_ = TRUE,
      timeout_ = missing_arg(),
      allow_no_input_binding_ = FALSE,
      priority_ = c("input", "event")
    )

#### Arguments

- `...`:

  Name-value pairs,
  `component_name_1 = value_1, component_name_2 = value_2` etc. Input
  with name `component_name_1` will be assigned value `value_1`.

- `wait_`:

  Wait until all reactive updates have completed?

- `timeout_`:

  Amount of time to wait before giving up (milliseconds). Defaults to
  the resolved `timeout` value during the `AppDriver` initialization.

- `allow_no_input_binding_`:

  When setting the value of an input, allow it to set the value of an
  input even if that input does not have an input binding. This is
  useful to replicate behavior like hovering over a plotly plot.

- `priority_`:

  Sets the event priority. For expert use only: see
  <https://shiny.rstudio.com/articles/communicating-with-js.html#values-vs-events>
  for details.

#### Examples

    \dontrun{
    app_path <- system.file("examples/07_widgets", package = "shiny")
    app <- AppDriver$new(app_path)

    cat(app$get_text("#view"))
    app$set_inputs(dataset = "cars", obs = 6)
    app$click("update")
    cat(app$get_text("#view"))
    }

------------------------------------------------------------------------

### Method `upload_file()`

Upload a file

Uploads a file to the specified file input.

#### Usage

    AppDriver$upload_file(..., wait_ = TRUE, timeout_ = missing_arg())

#### Arguments

- `...`:

  Name-path pair, e.g. `component_name = file_path`. The file located at
  `file_path` will be uploaded to file input with name `component_name`.

- `wait_`:

  Wait until all reactive updates have completed?

- `timeout_`:

  Amount of time to wait before giving up (milliseconds). Defaults to
  the resolved `timeout` value during the `AppDriver` initialization.

#### Examples

    \dontrun{
    app_path <- system.file("examples/09_upload", package = "shiny")
    app <- AppDriver$new(app_path)

    # Save example file
    tmpfile <- tempfile(fileext = ".csv")
    write.csv(cars, tmpfile, row.names = FALSE)

    # Upload file to input named: file1
    app$upload_file(file1 = tmpfile)
    }

------------------------------------------------------------------------

### Method `expect_values()`

Expect `input`, `output`, and `export` values

A JSON snapshot is saved of given the results from the underlying call
to `$get_values()`.

Note, values that contain environments or other values that will have
trouble serializing may not work well. Instead, these objects should be
manually inspected and have their components tested individually.

Please see [Robust
testing](https://rstudio.github.io/shinytest2/articles/robust.html) for
more details.

#### Usage

    AppDriver$expect_values(
      ...,
      input = missing_arg(),
      output = missing_arg(),
      export = missing_arg(),
      screenshot_args = missing_arg(),
      name = NULL,
      transform = NULL,
      cran = deprecated()
    )

#### Arguments

- `...`:

  Must be empty. Allows for parameter expansion.

- `input, output, export`:

  Depending on which parameters are supplied, different return values
  can occur: \* If `input`, `output`, and `export` are all missing, then
  all values are included in the snapshot. \* If at least one `input`,
  `output`, or `export` is specified, then only the requested values are
  included in the snapshot.

  The values supplied to each variable can be: \* A character vector of
  specific names to only include in the snapshot. \* `TRUE` to request
  that all values of that type are included in the snapshot. \* Anything
  else (e.g. `NULL` or `FALSE`) will result in the parameter being
  ignored.

- `screenshot_args`:

  This value is passed along to `$expect_screenshot()` where the
  resulting snapshot expectation is ignored. If missing, the default
  value will be `$new(expect_values_screenshot_args=)`.

  The final value can either be:

  - `TRUE`: A screenshot of the browser's scrollable area will be taken
    with no delay

  - `FALSE`: No screenshot will be taken

  - A named list of arguments. These arguments are passed directly to
    [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)'s
    `$get_screenshot()` method. The `selector` and `delay` will default
    to `"html"` and `0` respectively.

- `name`:

  The file name to be used for the snapshot. The file extension will be
  overwritten to `.json`. By default, the `name` supplied to `app` on
  initialization with a counter will be used (e.g. `"NAME-001.json"`).

- `transform`:

  Optionally, a function to scrub sensitive or stochastic text from the
  output. Should take a character vector of lines as input and return a
  modified character vector as output.

- `cran`:

  Deprecated. With `AppDriver` never testing on CRAN, this parameter no
  longer has any effect.

#### Returns

The result of the snapshot expectation

#### Examples

    \dontrun{
    library(shiny)
    shiny_app <- shinyApp(
      fluidPage(
        h1("Pythagorean theorem"),
        numericInput("A", "A", 3),
        numericInput("B", "B", 4),
        verbatimTextOutput("C"),
      ),
      function(input, output) {
        a_squared <- reactive({ req(input$A); input$A * input$A })
        b_squared <- reactive({ req(input$B); input$B * input$B })
        c_squared <- reactive({ a_squared() + b_squared() })
        c_value <- reactive({ sqrt(c_squared()) })
        output$C <- renderText({ c_value() })

        exportTestValues(
          a_squared = { a_squared() },
          b_squared = { b_squared() },
          c_squared = { c_squared() }
        )
      }
    )

    app <- AppDriver$new(shiny_app)

    # Snapshot all known values
    app$expect_values()

    # Snapshot only `export` values
    app$expect_values(export = TRUE)

    # Snapshot values `"A"` from `input` and `"C"` from `output`
    app$expect_values(input = "A", output = "C")
    }

------------------------------------------------------------------------

### Method `get_value()`

Get a single `input`, `output`, or `export` value

This is a helper function around `$get_values()` to retrieve a single
`input`, `output`, or `export` value. Only a single `input`, `output`,
or `export` value can be used.

Note, values that contain environments or other values that will have
trouble serializing to RDS may not work well.

#### Usage

    AppDriver$get_value(
      ...,
      input = missing_arg(),
      output = missing_arg(),
      export = missing_arg(),
      hash_images = FALSE
    )

#### Arguments

- `...`:

  Must be empty. Allows for parameter expansion.

- `input, output, export`:

  One of these variable should contain a single string value. If more
  than one value is specified or no values are specified, an error will
  be thrown.

- `hash_images`:

  If `TRUE`, images will be hashed before being returned. Otherwise, all
  images will return their full data64 encoded value.

#### Returns

The requested `input`, `output`, or `export` value.

#### Examples

    \dontrun{
    app_path <- system.file("examples/04_mpg", package = "shiny")
    app <- AppDriver$new(app_path)

    # Retrieve a single value
    app$get_value(output = "caption")
    #> [1] "mpg ~ cyl"
    # Equivalent code using `$get_values()`
    app$get_values(output = "caption")$output$caption
    #> [1] "mpg ~ cyl"
    }

------------------------------------------------------------------------

### Method `get_values()`

Get `input`, `output`, and `export` values

Retrieves a list of all known `input`, `output`, or `export` values.
This method is a core method when inspecting your Shiny app.

Note, values that contain environments or other values that will have
trouble serializing may not work well.

#### Usage

    AppDriver$get_values(
      ...,
      input = missing_arg(),
      output = missing_arg(),
      export = missing_arg(),
      hash_images = FALSE
    )

#### Arguments

- `...`:

  Must be empty. Allows for parameter expansion.

- `input, output, export`:

  Depending on which parameters are supplied, different return values
  can occur: \* If `input`, `output`, and `export` are all missing, then
  all values are included in the snapshot. \* If at least one `input`,
  `output`, or `export` is specified, then only the requested values are
  included in the snapshot.

  The values supplied to each variable can be: \* A character vector of
  specific names to only include in the snapshot. \* `TRUE` to request
  that all values of that type are included in the snapshot. \* Anything
  else (e.g. `NULL` or `FALSE`) will result in the parameter being
  ignored.

- `hash_images`:

  If `TRUE`, images will be hashed before being returned. Otherwise, all
  images will return their full data64 encoded value.

#### Returns

A named list of all inputs, outputs, and export values.

#### Examples

    \dontrun{
    library(shiny)
    shiny_app <- shinyApp(
      fluidPage(
        h1("Pythagorean theorem"),
        numericInput("A", "A", 3),
        numericInput("B", "B", 4),
        verbatimTextOutput("C"),
      ),
      function(input, output) {
        a_squared <- reactive({ req(input$A); input$A * input$A })
        b_squared <- reactive({ req(input$B); input$B * input$B })
        c_squared <- reactive({ a_squared() + b_squared() })
        c_value <- reactive({ sqrt(c_squared()) })
        output$C <- renderText({ c_value() })

        exportTestValues(
          a_squared = { a_squared() },
          b_squared = { b_squared() },
          c_squared = { c_squared() }
        )
      }
    )

    app <- AppDriver$new(shiny_app)

    # Show all known values
    str(app$get_values())
    #> List of 3
    #> $ input :List of 2
    #> ..$ A: int 3
    #> ..$ B: int 4
    #> $ output:List of 1
    #> ..$ C: chr "5"
    #> $ export:List of 3
    #> ..$ a_squared: int 9
    #> ..$ b_squared: int 16
    #> ..$ c_squared: int 25

    # Get only `export` values
    str(app$get_values(export = TRUE))
    #> List of 1
    #> $ export:List of 3
    #> ..$ a_squared: int 9
    #> ..$ b_squared: int 16
    #> ..$ c_squared: int 25

    # Get values `"A"` from `input` and `"C"` from `output`
    str(app$get_values(input = "A", output = "C"))
    #> List of 2
    #> $ input :List of 1
    #> ..$ A: int 3
    #> $ output:List of 1
    #> ..$ C: chr "5"
    }

------------------------------------------------------------------------

### Method `expect_download()`

Expect a downloadable file

Given a
[`shiny::downloadButton()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)/[`shiny::downloadLink()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)
`output` ID, the corresponding file will be downloaded and saved as a
snapshot file.

#### Usage

    AppDriver$expect_download(
      output,
      ...,
      compare = NULL,
      name = NULL,
      transform = NULL,
      cran = deprecated()
    )

#### Arguments

- `output`:

  `output` ID of
  [`shiny::downloadButton()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)/[`shiny::downloadLink()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)

- `...`:

  Must be empty. Allows for parameter expansion.

- `compare`:

  This value is passed through to
  [`testthat::expect_snapshot_file()`](https://testthat.r-lib.org/reference/expect_snapshot_file.html).
  By default it is set to `NULL` which will default to
  [`testthat::compare_file_text`](https://testthat.r-lib.org/reference/expect_snapshot_file.html)
  if `name` has extension `.r`, `.R`, `.Rmd`, `.md`, or `.txt`, and
  otherwise uses
  [`testthat::compare_file_binary`](https://testthat.r-lib.org/reference/expect_snapshot_file.html).

- `name`:

  File name to save file to (including file name extension). The
  default, `NULL`, generates an ascending sequence of names:
  `001.download`, `002.download`, etc.

- `transform`:

  Optionally, a function to scrub sensitive or stochastic text from the
  output. Should take a character vector of lines as input and return a
  modified character vector as output.

- `cran`:

  Deprecated. With `AppDriver` never testing on CRAN, this parameter no
  longer has any effect.

#### Examples

    \dontrun{
    app_path <- system.file("examples/10_download", package = "shiny")
    app <- AppDriver$new(app_path)

    # Save snapshot of rock.csv as 001.download
    # Save snapshot value of `rock.csv` to capture default file name
    app$expect_download("downloadData", compare = testthat::compare_file_text)
    }

------------------------------------------------------------------------

### Method `get_download()`

Get downloadable file

Given a
[`shiny::downloadButton()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)/[`shiny::downloadLink()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)
`output` ID, the corresponding file will be downloaded and saved as a
file.

#### Usage

    AppDriver$get_download(output, filename = NULL)

#### Arguments

- `output`:

  `output` ID of
  [`shiny::downloadButton()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)/[`shiny::downloadLink()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)

- `filename`:

  File path to save the downloaded file to.

#### Returns

`$get_download()` will return the final save location of the file. This
location can change depending on the value of `filename` and response
headers.

Location logic:

- If `filename` is not NULL, `filename` will be returned.

- If a [`content-disposition`
  `filename`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition)
  is provided, then a temp file containing this `filename` will be
  returned.

- Otherwise, a temp file ending in `.download` will be returned.

#### Examples

    \dontrun{
    app_path <- system.file("examples/10_download", package = "shiny")
    app <- AppDriver$new(app_path)

    # Get rock.csv as a tempfile
    app$get_download("downloadData")
    #> [1] "/TEMP/PATH/rock.csv"

    # Get rock.csv as a "./myfile.csv"
    app$get_download("downloadData", filename = "./myfile.csv")
    #> [1] "./myfile.csv"
    }

------------------------------------------------------------------------

### Method `expect_text()`

Expect snapshot of UI text

`$expect_text()` will extract the text value of all matching elements
via `TAG.textContent` and store them in a snapshot file. This method is
more robust to internal package change as only the text values will be
maintained. Note, this method will not retrieve any `<input />` value's
text content, e.g. text inputs or text areas, as the input values are
not stored in the live HTML.

When possible, use `$expect_text()` over `$expect_html()` to allow
package authors room to alter their HTML structures. The resulting array
of `TAG.textContent` values found will be stored in a snapshot file.

Please see [Robust
testing](https://rstudio.github.io/shinytest2/articles/robust.html) for
more details.

#### Usage

    AppDriver$expect_text(selector, ..., cran = deprecated())

#### Arguments

- `selector`:

  A DOM CSS selector to be passed into `document.querySelectorAll()`

- `...`:

  Must be empty. Allows for parameter expansion.

- `cran`:

  Deprecated. With `AppDriver` never testing on CRAN, this parameter no
  longer has any effect.

#### Examples

    \dontrun{
    hello_app <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(hello_app)

    # Make a snapshot of `"Hello Shiny!"`
    app$expect_text("h2")
    }

------------------------------------------------------------------------

### Method `get_text()`

Get UI text

`$get_text()` will extract the text value of all matching elements via
`TAG.textContent`. Note, this method will not retrieve any `<input />`
value's text content, e.g. text inputs or text areas, as the input
values are not stored in the live HTML.

#### Usage

    AppDriver$get_text(selector)

#### Arguments

- `selector`:

  A DOM CSS selector to be passed into `document.querySelectorAll()`

#### Returns

A vector of character values

#### Examples

    \dontrun{
    hello_app <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(hello_app)

    app$get_text("h2")
    #> [1] "Hello Shiny!"
    }

------------------------------------------------------------------------

### Method `expect_html()`

Expect snapshot of UI HTML

`$expect_html()` will extract the full DOM structures of each matching
element and store them in a snapshot file. This method captures internal
DOM structure which may be brittle to changes by external authors or
dependencies.

Note, this method will not retrieve any `<input />` value's text
content, e.g. text inputs or text areas, as the input values are not
stored in the live HTML.

When possible, use `$expect_text()` over `$expect_html()` to allow
package authors room to alter their HTML structures. The resulting array
of `TAG.textContent` values found will be stored in a snapshot file.

Please see [Robust
testing](https://rstudio.github.io/shinytest2/articles/robust.html) for
more details.

#### Usage

    AppDriver$expect_html(selector, ..., outer_html = TRUE, cran = deprecated())

#### Arguments

- `selector`:

  A DOM selector to be passed into `document.querySelectorAll()`

- `...`:

  Must be empty. Allows for parameter expansion.

- `outer_html`:

  If `TRUE` (default), the full DOM structure will be returned
  (`TAG.outerHTML`). If `FALSE`, the full DOM structure of the child
  elements will be returned (`TAG.innerHTML`).

- `cran`:

  Deprecated. With `AppDriver` never testing on CRAN, this parameter no
  longer has any effect.

#### Examples

    \dontrun{
    app_path <- system.file("examples/04_mpg", package = "shiny")
    app <- AppDriver$new(app_path)

    # Save a snapshot of the `caption` output
    app$expect_html("#caption")
    }

------------------------------------------------------------------------

### Method `get_html()`

Get UI HTML

`$get()` will extract the full DOM structures of each matching element.
This method captures internal DOM structure which may be brittle to
changes by external authors or dependencies.

Note, this method will not retrieve any `<input />` value's text
content, e.g. text inputs or text areas, as the input values are not
stored in the live HTML.

Please see [Robust
testing](https://rstudio.github.io/shinytest2/articles/robust.html) for
more details.

#### Usage

    AppDriver$get_html(selector, ..., outer_html = TRUE)

#### Arguments

- `selector`:

  A DOM selector to be passed into `document.querySelectorAll()`

- `...`:

  Must be empty. Allows for parameter expansion.

- `outer_html`:

  If `TRUE`, the full DOM structure will be returned (`TAG.outerHTML`).
  If `FALSE`, the full DOM structure of the child elements will be
  returned (`TAG.innerHTML`).

#### Examples

    \dontrun{
    app_path <- system.file("examples/03_reactivity", package = "shiny")
    app <- AppDriver$new(app_path, check_names = FALSE)

    app$set_inputs(caption = "Custom value!")
    cat(app$get_html(".shiny-input-container")[1])
    #> <div class="form-group shiny-input-container">
    #>   <label class="control-label" id="caption-label" for="caption">Caption:</label>
    #>   <input id="caption" type="text" class="form-control shiny-bound-input" value="Data Summary">
    #> </div>
    ## ^^ No update to the DOM of `caption`
    }

------------------------------------------------------------------------

### Method `expect_js()`

Expect snapshot of JavaScript script output

This is a building block function that may be called by other functions.
For example, `$expect_text()` and `$expect_html()` are thin wrappers
around this function.

Once the `script` has executed, the JSON result will be saved to a
snapshot file.

#### Usage

    AppDriver$expect_js(
      script = missing_arg(),
      ...,
      file = missing_arg(),
      timeout = missing_arg(),
      pre_snapshot = NULL,
      cran = deprecated()
    )

#### Arguments

- `script`:

  A string containing the JavaScript script to be executed.

- `...`:

  Must be empty. Allows for parameter expansion.

- `file`:

  A file containing JavaScript code to be read and used as the `script`.
  Only one of `script` or `file` can be specified.

- `timeout`:

  Amount of time to wait before giving up (milliseconds). Defaults to
  the resolved `timeout` value during the `AppDriver` initialization.

- `pre_snapshot`:

  A function to be called on the result of the script before taking the
  snapshot. `$expect_html()` and `$expect_text()` both use
  [`unlist()`](https://rdrr.io/r/base/unlist.html).

- `cran`:

  Deprecated. With `AppDriver` never testing on CRAN, this parameter no
  longer has any effect.

#### Examples

    \dontrun{
    app_path <- system.file("examples/07_widgets", package = "shiny")
    app <- AppDriver$new(app_path)

    # Track how many clicks are given to `#update` button
    app$run_js("
      window.test_counter = 0;
      $('#update').click(() => window.test_counter++);
    ")
    app$set_inputs(obs = 20)
    # Click the update button, incrementing the counter
    app$click("update")
    # Save a snapshot of number of clicks (1)
    app$expect_js("window.test_counter;")
    }

------------------------------------------------------------------------

### Method `get_js()`

Execute JavaScript code in the browser and return the result

This function will block the local R session until the code has finished
executing its *tick* in the browser. If a `Promise` is returned from the
script, `$get_js()` will wait for the promise to resolve. To have
JavaScript code execute asynchronously, wrap the code in a Promise
object and have the script return an atomic value.

Arguments will have to be inserted into the script as there is not
access to `arguments`. This can be done with commands like
[`paste()`](https://rdrr.io/r/base/paste.html). If using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html), be
sure to use uncommon `.open` and `.close` values to avoid having to
double all `{` and `}`.

#### Usage

    AppDriver$get_js(
      script = missing_arg(),
      ...,
      file = missing_arg(),
      timeout = missing_arg()
    )

#### Arguments

- `script`:

  JavaScript to execute. If a JavaScript Promise is returned, the R
  session will block until the promise has been resolved and return the
  value.

- `...`:

  Must be empty. Allows for parameter expansion.

- `file`:

  A (local) file containing JavaScript code to be read and used as the
  `script`. Only one of `script` or `file` can be specified.

- `timeout`:

  Amount of time to wait before giving up (milliseconds). Defaults to
  the resolved `timeout` value during the `AppDriver` initialization.

#### Returns

Result of the `script` (or `file` contents)

#### Examples

    \dontrun{
    library(shiny)
    shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
    app <- AppDriver$new(shiny_app)

    # Execute JavaScript code in the app's browser
    app$get_js("1 + 1;")
    #> [1] 2

    # Execute a JavaScript Promise. Return the resolved value.
    app$get_js("
      new Promise((resolve) => {
        setTimeout(() => resolve(1 + 1), 1000)
      }).
      then((value) => value + 1);
    ")
    #> [1] 3

    # With escaped arguments
    loc_field <- "hostname"
    js_txt <- paste0("window.location[", jsonlite::toJSON(loc_field, auto_unbox = TRUE), "]")
    app$get_js(js_txt)
    #> [1] "127.0.0.1"

    # With `glue::glue()`
    js_txt <- glue::glue_data(
      lapply(
        list(x = 40, y = 2),
        jsonlite::toJSON,
        auto_unbox = TRUE
      ),
      .open = "<", .close = ">",
      "let answer = function(a, b) {\n",
      "  return a + b;\n",
      "};\n",
      "answer(<x>, <y>);\n"
    )
    app$get_js(js_txt)
    #> [1] 42
    }

------------------------------------------------------------------------

### Method `run_js()`

Execute JavaScript code in the browser

This function will block the local R session until the code has finished
executing its *tick* in the browser.

The final result of the code will be ignored and not returned to the R
session.

#### Usage

    AppDriver$run_js(
      script = missing_arg(),
      ...,
      file = missing_arg(),
      timeout = missing_arg()
    )

#### Arguments

- `script`:

  JavaScript to execute.

- `...`:

  Must be empty. Allows for parameter expansion.

- `file`:

  A (local) file containing JavaScript code to be read and used as the
  `script`. Only one of `script` or `file` can be specified.

- `timeout`:

  Amount of time to wait before giving up (milliseconds). Defaults to
  the resolved `timeout` value during the `AppDriver` initialization.

#### Examples

    \dontrun{
    library(shiny)
    shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
    app <- AppDriver$new(shiny_app)

    # Get JavaScript answer from the app's browser
    app$get_js("1 + 1")
    #> [1] 2
    # Execute JavaScript code in the app's browser
    app$run_js("1 + 1")
    # (Returns `app` invisibly)

    # With escaped arguments
    loc_field <- "hostname"
    js_txt <- paste0("window.location[", jsonlite::toJSON(loc_field, auto_unbox = TRUE), "]")
    app$run_js(js_txt)
    app$get_js(js_txt)
    #> [1] "127.0.0.1"
    }

------------------------------------------------------------------------

### Method `expect_screenshot()`

Expect a screenshot of the Shiny application

This method takes a screenshot of the application (of only the
`selector` area) and compares the image to the expected image.

Please be aware that this method is very brittle to changes outside of
your Shiny application. These changes can include:

- running on a different R version

- running on a different in operating system

- using a different default system font

- using different package versions These differences are explicitly
  clear when working with plots.

Unless absolutely necessary for application consistency, it is strongly
recommended to use other expectation methods.

#### Usage

    AppDriver$expect_screenshot(
      ...,
      threshold = getOption("shinytest2.compare_screenshot.threshold", NULL),
      kernel_size = getOption("shinytest2.compare_screenshot.kernel_size", 5),
      screenshot_args = missing_arg(),
      delay = missing_arg(),
      selector = missing_arg(),
      compare = missing_arg(),
      quiet = FALSE,
      name = NULL,
      cran = deprecated()
    )

#### Arguments

- `...`:

  Must be empty. Allows for parameter expansion.

- `threshold`:

  Parameter supplied to
  [`compare_screenshot_threshold()`](https://rstudio.github.io/shinytest2/reference/compare_screenshot_threshold.md)
  when using the default `compare` method. The default value can be set
  globally with the `shinytest2.compare_screenshot.threshold` option.

  If the value of `threshold` is `NULL`,
  [`compare_screenshot_threshold()`](https://rstudio.github.io/shinytest2/reference/compare_screenshot_threshold.md)
  will act like
  [`testthat::compare_file_binary`](https://testthat.r-lib.org/reference/expect_snapshot_file.html).
  However, if `threshold` is a positive number, it will be compared
  against the largest convolution value found if the two images fail a
  [`testthat::compare_file_binary`](https://testthat.r-lib.org/reference/expect_snapshot_file.html)
  comparison.

  Which value should I use? Threshold values values below 5 help deter
  false-positive screenshot comparisons (such as inconsistent rounded
  corners). Larger values in the 10s and 100s will help find *real*
  changes. However, not all values are one size fits all and you will
  need to play with a threshold that fits your needs.

- `kernel_size`:

  Parameter supplied to
  [`compare_screenshot_threshold()`](https://rstudio.github.io/shinytest2/reference/compare_screenshot_threshold.md)
  when using the default `compare` method. The `kernel_size` represents
  the height and width of the convolution kernel applied to the pixel
  differences. This integer-like value should be relatively small. The
  default value can be set globally with the
  `shinytest2.compare_screenshot.kernel_size` option.

- `screenshot_args`:

  This named list of arguments is passed along to
  [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)'s
  `$get_screenshot()` method. If missing, the value will default to
  `$new(screenshot_args=)`.

  If `screenshot_args` is:

  - `TRUE`: A screenshot of the browser's scrollable area will be taken
    with no delay

  - A named list of arguments: Arguments passed directly to
    [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)'s
    `$get_screenshot()` method. The `delay` argument will default to `0`
    seconds. The `selector` argument can take two special values in
    addition to being a CSS DOM selector.

    - `"scrollable_area"` (default): The entire scrollable area will be
      captured. Typically this is your browser's viewport size, but it
      can be larger if the page is scrollable. This value works well
      with Apps that contain elements whose calculated dimensions may be
      different than their presented size.

    - `"viewport"`: This value will capture the browser's viewport in
      its current viewing location, height, and width. It will only
      capture what is currently being seen with `$view()`.

    In `v0.3.0`, the default `selector` value was changed from the HTML
    DOM selector (`"html"`) to entire scrollable area
    (`"scrollable_area"`).

- `delay`:

  The number of **seconds** to wait before taking the screenshot. This
  value can be supplied as `delay` or `screenshot_args$delay`, with the
  `delay` parameter having preference.

- `selector`:

  The selector is a CSS selector that will be used to select a portion
  of the page to be captured. This value can be supplied as `selector`
  or `screenshot_args$selector`, with the `selector` parameter having
  preference.

  In `v0.3.0`, two special `selector` values were added:

  - `"scrollable_area"` (default): The entire scrollable area will be
    captured. Typically this is your browser's viewport size, but it can
    be larger if the page is scrollable. This value works well with Apps
    that contain elements whose calculated dimensions may be different
    than their presented size.

  - `"viewport"`: This value will capture the browser's viewport in its
    current viewing location, height, and width. It will only capture
    what is currently being seen with `$view()`.

  In `v0.3.0`, the default `selector` value was changed from the HTML
  DOM selector (`"html"`) to entire scrollable area
  (`"scrollable_area"`).

- `compare`:

  A function used to compare the screenshot snapshot files. The function
  should take two inputs, the paths to the `old` and `new` snapshot, and
  return either `TRUE` or `FALSE`.

  `compare` defaults to a function that wraps around
  `compare_screenshot_threshold(old, new, threshold = threshold, kernel_size = kernel_size, quiet = quiet)`.
  Note: if `threshold` is `NULL` (default), `compare` will behave as if
  [`testthat::compare_file_binary()`](https://testthat.r-lib.org/reference/expect_snapshot_file.html)
  was provided, comparing the two images byte-by-byte.

- `quiet`:

  Parameter supplied to
  [`compare_screenshot_threshold()`](https://rstudio.github.io/shinytest2/reference/compare_screenshot_threshold.md)
  when using the default `compare` method. If `FALSE`, diagnostic
  information will be presented when the computed value is larger than a
  non-`NULL` `threshold` value.

- `name`:

  The file name to be used for the snapshot. The file extension will
  overwritten to `.png`. By default, the `name` supplied to `app` on
  initialization with a counter will be used (e.g. `"NAME-001.png"`).

- `cran`:

  Deprecated. With `AppDriver` never testing on CRAN, this parameter no
  longer has any effect.

#### Examples

    \dontrun{
    # These example lines should be performed in a `./tests/testthat`
    # test file so that snapshot files can be properly saved

    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path, variant = platform_variant())

    # Expect a full size screenshot to be pixel perfect
    app$expect_screenshot()

    # Images are brittle when containing plots
    app$expect_screenshot(selector = "#distPlot")

    # Test with more threshold in pixel value differences
    # Helps with rounded corners
    app$expect_screenshot(threshold = 10)

    # Equivalent expectations
    app$expect_screenshot() # default
    app$expect_screenshot(threshold = NULL)
    app$expect_screenshot(compare = testthat::compare_file_binary)
    expect_snapshot_file(
      app$get_screenshot(),
      variant = app$get_variant(),
      compare = testthat::compare_file_binary
    )

    # Equivalent expectations
    app$expect_screenshot(threshold = 3, kernel_size = 5)
    app$expect_screenshot(compare = function(old, new) {
      compare_screenshot_threshold(
        old, new,
        threshold = 3,
        kernel_size = 5
      )
    })
    expect_screenshot_file(
      app$get_screenshot(),
      variant = app$get_variant(),
      compare = function(old, new) {
        compare_screenshot_threshold(
          old, new,
          threshold = 3,
          kernel_size = 5
        )
      }
    )

    # Take a screenshot of the entire scrollable area
    app$expect_screenshot()
    app$expect_screenshot(selector = "scrollable_area")

    ## Take a screenshot of the current viewport
    # Shrink the window to be smaller than the app
    app$set_window_size(400, 500)
    # Scroll the viewport just a bit
    app$run_js("window.scroll(30, 70)")
    # Take screenshot of browser viewport
    app$expect_screenshot(selector = "viewport")
    }

------------------------------------------------------------------------

### Method `get_screenshot()`

Take a screenshot

Take a screenshot of the Shiny application.

#### Usage

    AppDriver$get_screenshot(
      file = NULL,
      ...,
      screenshot_args = missing_arg(),
      delay = missing_arg(),
      selector = missing_arg()
    )

#### Arguments

- `file`:

  If `NULL`, then the image will be displayed to the current Graphics
  Device. If a file path, then the screenshot will be saved to that
  file.

- `...`:

  Must be empty. Allows for parameter expansion.

- `screenshot_args`:

  This named list of arguments is passed along to
  [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)'s
  `$get_screenshot()` method. If missing, the value will default to
  `$new(screenshot_args=)`.

  If `screenshot_args` is:

  - `TRUE`: A screenshot of the browser's scrollable area will be taken
    with no delay

  - A named list of arguments: Arguments passed directly to
    [`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)'s
    `$get_screenshot()` method. The `delay` argument will default to `0`
    seconds. The `selector` argument can take two special values in
    addition to being a CSS DOM selector.

    - `"scrollable_area"` (default): The entire scrollable area will be
      captured. Typically this is your browser's viewport size, but it
      can be larger if the page is scrollable. This value works well
      with Apps that contain elements whose calculated dimensions may be
      different than their presented size.

    - `"viewport"`: This value will capture the browser's viewport in
      its current viewing location, height, and width. It will only
      capture what is currently being seen with `$view()`.

    In `v0.3.0`, the default `selector` value was changed from the HTML
    DOM selector (`"html"`) to entire scrollable area
    (`"scrollable_area"`).

  If `screenshot_args=FALSE` is provided, the parameter will be ignored
  and a screenshot will be taken with default behavior.

- `delay`:

  The number of **seconds** to wait before taking the screenshot. This
  value can be supplied as `delay` or `screenshot_args$delay`, with the
  `delay` parameter having preference.

- `selector`:

  The selector is a CSS selector that will be used to select a portion
  of the page to be captured. This value can be supplied as `selector`
  or `screenshot_args$selector`, with the `selector` parameter having
  preference.

  In `v0.3.0`, two special `selector` values were added:

  - `"scrollable_area"` (default): The entire scrollable area will be
    captured. Typically this is your browser's viewport size, but it can
    be larger if the page is scrollable. This value works well with Apps
    that contain elements whose calculated dimensions may be different
    than their presented size.

  - `"viewport"`: This value will capture the browser's viewport in its
    current viewing location, height, and width. It will only capture
    what is currently being seen with `$view()`.

  In `v0.3.0`, the default `selector` value was changed from the HTML
  DOM selector (`"html"`) to entire scrollable area
  (`"scrollable_area"`).

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path)

    # Display in graphics device
    app$get_screenshot()

    # Update bins then display `"disPlot"` in graphics device
    app$set_inputs(bins = 10)
    app$get_screenshot(selector = "#distPlot")

    # Save screenshot to file and view it
    tmpfile <- tempfile(fileext = ".png")
    app$get_screenshot(tmpfile)
    showimage::show_image(tmpfile)
    }

------------------------------------------------------------------------

### Method `wait_for_idle()`

Wait for Shiny to not be busy (idle) for a set amount of time

Waits until Shiny has not been busy for a set duration of time, e.g. no
reactivity is updating or has occurred.

This is useful, for example, when waiting for your application to
initialize or if you've resized the window with `$set_window_size()` and
want to make sure all plot redrawing is complete before take a
screenshot.

By default,

- `$new(wait = TRUE)` waits for Shiny to not be busy after initializing
  the application

- `$set_window_size(wait = TRUE)` waits for Shiny to not be busy after
  resizing the window.)

#### Usage

    AppDriver$wait_for_idle(duration = 500, timeout = missing_arg())

#### Arguments

- `duration`:

  How long Shiny must be idle (in ms) before unblocking the R session.

- `timeout`:

  Amount of time to wait before giving up (milliseconds). Defaults to
  the resolved `timeout` value during the `AppDriver` initialization.

#### Returns

`invisible(self)` if Shiny stabilizes within the `timeout`. Otherwise an
error will be thrown

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path)

    pre_value <- app$get_value(output = "distPlot")
    # Update bins value
    app$set_inputs(bins = 10, wait_ = FALSE)
    middle_value <- app$get_value(output = "distPlot")
    app$wait_for_idle()
    post_value <- app$get_value(output = "distPlot")

    # No guarantee that these values are different
    identical(pre_value, middle_value)
    # Will not be equal
    identical(pre_value, post_value)

    # ---------------------
    ## Change the screen size to trigger a plot update
    pre_value <- app$get_value(output = "distPlot")
    app$set_window_size(height = 1080, width = 1920, wait = FALSE)
    middle_value <- app$get_value(output = "distPlot")
    app$wait_for_idle()
    post_value <- app$get_value(output = "distPlot")

    # No guarantee that these values are different
    identical(pre_value, middle_value)
    # Will not be equal
    identical(pre_value, post_value)
    }

------------------------------------------------------------------------

### Method `wait_for_value()`

Wait for a new Shiny value

Waits until an `input`, `output`, or `export` Shiny value is not one of
`ignore`d values, or the `timeout` is reached.

Only a single `input`, `output`, or `export` value may be used.

This function can be useful in helping determine if an application has
finished processing a complex reactive situation.

#### Usage

    AppDriver$wait_for_value(
      ...,
      input = missing_arg(),
      output = missing_arg(),
      export = missing_arg(),
      ignore = list(NULL, ""),
      timeout = missing_arg(),
      interval = 400
    )

#### Arguments

- `...`:

  Must be empty. Allows for parameter expansion.

- `input, output, export`:

  A name of an input, output, or export value. Only one of these
  parameters may be used.

- `ignore`:

  List of possible values to ignore when checking for updates.

- `timeout`:

  Amount of time to wait before giving up (milliseconds). Defaults to
  the resolved `timeout` value during the `AppDriver` initialization.

- `interval`:

  How often to check for the condition, in ms.

- `timeout_`:

  Amount of time to wait for a new `output` value before giving up
  (milliseconds). Defaults to the resolved `timeout` value during the
  `AppDriver` initialization.

#### Returns

Newly found value

#### Examples

    \dontrun{
    library(shiny)
    shiny_app <- shinyApp(
      fluidPage(
        h1("Dynamic output"),
        actionButton("display", "Display UI"),
        uiOutput("dym1"),
      ),
      function(input, output) {
        output$dym1 <- renderUI({
          req(input$display)
          Sys.sleep(runif(1, max = 2)) # Artificial calculations
          tagList(
            sliderInput("slider1", "Slider #1", 0, 100, 25),
            uiOutput("dym2")
          )
        })
        output$dym2 <- renderUI({
          Sys.sleep(runif(1, max = 2)) # Artificial calculations
          tagList(
            sliderInput("slider2", "Slider #2", 0, 100, 50),
            "Total:", verbatimTextOutput("total")
          )
        })
        output$total <- renderText({
          req(input$slider1, input$slider2)
          input$slider1 + input$slider2
        })
      }
    )

    app <- AppDriver$new(shiny_app)

    # Create UI / output values
    app$click("display")
    # Wait for total to be calculated (or have a non-NULL value)
    new_total_value <- app$wait_for_value(output = "total")
    #> [1] "75"
    app$get_value(output = "total")
    #> [1] "75"
    }

------------------------------------------------------------------------

### Method `wait_for_js()`

Wait for a JavaScript expression to be true

Waits until a JavaScript `expr`ession evaluates to `true` or the
`timeout` is exceeded.

#### Usage

    AppDriver$wait_for_js(script, timeout = missing_arg(), interval = 100)

#### Arguments

- `script`:

  A string containing JavaScript code. This code must eventually return
  a [`true`thy
  value](https://developer.mozilla.org/en-US/docs/Glossary/Truthy) or a
  timeout error will be thrown.

- `timeout`:

  How long the script has to return a `true`thy value (milliseconds).
  Defaults to the resolved `timeout` value during the `AppDriver`
  initialization.

- `interval`:

  How often to check for the condition (milliseconds).

#### Returns

`invisible(self)` if expression evaluates to `true` without error within
the timeout. Otherwise an error will be thrown

#### Examples

    \dontrun{
    shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
    app <- AppDriver$new(shiny_app)

    # Contrived example:
    # Wait until `Date.now()` returns a number that ends in a 5. (0 - 10 seconds)
    system.time(
      app$wait_for_js("Math.floor((Date.now() / 1000) % 10) == 5;")
    )

    ## A second example where we run the contents of a JavaScript file
    ## and use the result to wait for a condition
    app$run_js(file = "complicated_file.js")
    app$wait_for_js("complicated_condition();")
    }

------------------------------------------------------------------------

### Method `expect_unique_names()`

Expect unique input and output names.

If the HTML has duplicate input or output elements with matching `id`
values, this function will throw an error. It is similar to
`AppDriver$new(check_names = TRUE)`, but asserts that no warnings are
displayed.

This method will not throw if a single input and a single output have
the same name.

#### Usage

    AppDriver$expect_unique_names()

#### Examples

    \dontrun{
    shiny_app <- shinyApp(
      ui = fluidPage(
        # Duplicate input IDs: `"text"`
        textInput("text", "Text 1"),
        textInput("text", "Text 2")
      ),
      server = function(input, output) {
        # empty
      }
    )
    # Initial checking for unique names (default behavior)
    app <- AppDriver$new(shiny_app, check_names = TRUE)
    #> Warning:
    #> ! Shiny inputs should have unique HTML id values.
    #> i The following HTML id values are not unique:
    #> • text
    app$stop()

    # Manually assert that all names are unique
    app <- AppDriver$new(shiny_app, check_names = FALSE)
    app$expect_unique_names()
    #> Error: `app_check_unique_names(self, private)` threw an unexpected warning.
    #> Message: ! Shiny inputs should have unique HTML id values.
    #> i The following HTML id values are not unique:
    #>   • text
    #> Class:   rlang_warning/warning/condition
    app$stop()
    }

------------------------------------------------------------------------

### Method `get_dir()`

Retrieve the Shiny app path

#### Usage

    AppDriver$get_dir()

#### Returns

The directory containing the Shiny application or Shiny runtime
document. If a URL was provided to `app_dir` during initialization, the
current directory will be returned.

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path)

    identical(app$get_dir(), app_path)
    #> [1] TRUE
    }

------------------------------------------------------------------------

### Method `get_url()`

Retrieve the Shiny app URL

#### Usage

    AppDriver$get_url()

#### Returns

URL where the Shiny app is being hosted

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path)

    browseURL(app$get_url())
    }

------------------------------------------------------------------------

### Method `get_window_size()`

Get window size

Get current size of the browser window, as list of numeric scalars named
`width` and `height`.

#### Usage

    AppDriver$get_window_size()

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path)

    app$get_window_size()
    #> $width
    #> [1] 992
    #>
    #> $height
    #> [1] 1323
    }

------------------------------------------------------------------------

### Method `set_window_size()`

Sets size of the browser window.

#### Usage

    AppDriver$set_window_size(width, height, wait = TRUE)

#### Arguments

- `width, height`:

  Height and width of browser, in pixels.

- `wait`:

  If `TRUE`, `$wait_for_idle()` will be called after setting the window
  size. This will block until any width specific items (such as plots)
  that need to be re-rendered.

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    # Set init window size
    app <- AppDriver$new(app_path, height = 1400, width = 1000)

    app$get_window_size()
    #> $width
    #> [1] 1000
    #>
    #> $height
    #> [1] 1400

    # Manually set the window size
    app$set_window_size(height = 1080, width = 1920)
    app$get_window_size()
    #> $width
    #> [1] 1920
    #>
    #> $height
    #> [1] 1080
    }

------------------------------------------------------------------------

### Method `get_chromote_session()`

Get Chromote Session

Get the
[`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)
object from the chromote package.

#### Usage

    AppDriver$get_chromote_session()

#### Returns

[`chromote::ChromoteSession`](https://rstudio.github.io/chromote/reference/ChromoteSession.html)
R6 object

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path)

    b <- app$get_chromote_session()
    b$Runtime$evaluate("1 + 1")
    #> $result
    #> $result$type
    #> [1] "number"
    #>
    #> $result$value
    #> [1] 2
    #>
    #> $result$description
    #> [1] "2"
    }

------------------------------------------------------------------------

### Method `get_variant()`

Get the variant

Get the `variant` supplied during initialization

#### Usage

    AppDriver$get_variant()

#### Returns

The `variant` value supplied during initialization or `NULL` if no value
was supplied.

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")

    app <- AppDriver$new(app_path)

    app$get_variant()
    #> NULL

    app <- AppDriver$new(app_path, variant = platform_variant())
    app$get_variant()
    #> [1] "mac-4.1"
    }

------------------------------------------------------------------------

### Method `get_logs()`

Get all logs

Retrieve all of the debug logs that have been recorded.

#### Usage

    AppDriver$get_logs()

#### Returns

A data.frame with the following columns:

- `workerid`: The shiny worker ID found within the browser

- `timestamp`: POSIXct timestamp of the message

- `location`: The location of the message was found. One of three
  values:

  - `"shinytest2"`: Occurs when `$log_message()` is called

  - `"shiny"`: `stdin` and `stdout` messages from the Shiny server. Note
    [`message()`](https://rdrr.io/r/base/message.html) output is sent to
    `stdout`.

  - `"chromote"`: Captured by the chromote event handlers. See [console
    API](https://chromedevtools.github.io/devtools-protocol/1-3/Runtime/#event-consoleAPICalled),
    [exception
    thrown](https://chromedevtools.github.io/devtools-protocol/1-3/Runtime/#event-exceptionThrown),
    [websocket
    sent](https://chromedevtools.github.io/devtools-protocol/1-3/Network/#event-webSocketFrameSent),
    and [websocket
    received](https://chromedevtools.github.io/devtools-protocol/1-3/Network/#event-webSocketFrameReceived)
    for more details

- `level`: For a given location, there are different types of log
  levels.

  - `"shinytest2"`: `"log"`; Only log messages are captured.

  - `"shiny"`: `"stdout"` or `"stderr"`; Note,
    [`message()`](https://rdrr.io/r/base/message.html) output is sent to
    `stderr`.

  - `"chromote"`: Correspond to any level of a JavaScript
    `console.LEVEL()` function call. Typically, these are
    "log"`and`"error"`but can include`"info"`, `"debug"`, and `"warn"`. If `options(shiny.trace
    = TRUE)`, then the level will recorded as `"websocket"\`.

#### Examples

    \dontrun{
    app1 <- AppDriver$new(system.file("examples/01_hello", package = "shiny"))

    app1$get_logs()
    #> {shinytest2} R  info   10:00:28.86 Start AppDriver initialization
    #> {shinytest2} R  info   10:00:28.86 Starting Shiny app
    #> {shinytest2} R  info   10:00:29.76 Creating new ChromoteSession
    #> {shinytest2} R  info   10:00:30.56 Navigating to Shiny app
    #> {shinytest2} R  info   10:00:30.70 Injecting shiny-tracer.js
    #> {chromote}   JS info   10:00:30.75 shinytest2; jQuery found
    #> {chromote}   JS info   10:00:30.77 shinytest2; Waiting for shiny session to connect
    #> {chromote}   JS info   10:00:30.77 shinytest2; Loaded
    #> {shinytest2} R  info   10:00:30.77 Waiting for Shiny to become ready
    #> {chromote}   JS info   10:00:30.90 shinytest2; Connected
    #> {chromote}   JS info   10:00:30.95 shinytest2; shiny:busy
    #> {shinytest2} R  info   10:00:30.98 Waiting for Shiny to become idle for 200ms within 15000ms
    #> {chromote}   JS info   10:00:30.98 shinytest2; Waiting for Shiny to be stable
    #> {chromote}   JS info   10:00:31.37 shinytest2; shiny:idle
    #> {chromote}   JS info   10:00:31.38 shinytest2; shiny:value distPlot
    #> {chromote}   JS info   10:00:31.57 shinytest2; Shiny has been idle for 200ms
    #> {shinytest2} R  info   10:00:31.57 Shiny app started
    #> {shiny}      R  stderr ----------- Loading required package: shiny
    #> {shiny}      R  stderr ----------- Running application in test mode.
    #> {shiny}      R  stderr -----------
    #> {shiny}      R  stderr ----------- Listening on http://127.0.0.1:4679


    # To capture all websocket traffic, set `options = list(shiny.trace = TRUE)`
    app2 <- AppDriver$new(
      system.file("examples/01_hello", package = "shiny"),
      options = list(shiny.trace = TRUE)
    )

    app2$get_logs()
    ## (All WebSocket messages have been replaced with `WEBSOCKET_MSG` in example below)
    #> {shinytest2} R  info      10:01:57.49 Start AppDriver initialization
    #> {shinytest2} R  info      10:01:57.50 Starting Shiny app
    #> {shinytest2} R  info      10:01:58.20 Creating new ChromoteSession
    #> {shinytest2} R  info      10:01:58.35 Navigating to Shiny app
    #> {shinytest2} R  info      10:01:58.47 Injecting shiny-tracer.js
    #> {chromote}   JS info      10:01:58.49 shinytest2; jQuery not found
    #> {chromote}   JS info      10:01:58.49 shinytest2; Loaded
    #> {shinytest2} R  info      10:01:58.50 Waiting for Shiny to become ready
    #> {chromote}   JS info      10:01:58.55 shinytest2; jQuery found
    #> {chromote}   JS info      10:01:58.55 shinytest2; Waiting for shiny session to connect
    #> {chromote}   JS websocket 10:01:58.64 send WEBSOCKET_MSG
    #> {chromote}   JS websocket 10:01:58.67 recv WEBSOCKET_MSG
    #> {chromote}   JS info      10:01:58.67 shinytest2; Connected
    #> {chromote}   JS websocket 10:01:58.71 recv WEBSOCKET_MSG
    #> {chromote}   JS websocket 10:01:58.72 recv WEBSOCKET_MSG
    #> {chromote}   JS info      10:01:58.72 shinytest2; shiny:busy
    #> {chromote}   JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #> {chromote}   JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #> {shinytest2} R  info      10:01:58.75 Waiting for Shiny to become idle for 200ms within 15000ms
    #> {chromote}   JS info      10:01:58.75 shinytest2; Waiting for Shiny to be stable
    #> {chromote}   JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #> {chromote}   JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #> {chromote}   JS info      10:01:58.81 shinytest2; shiny:idle
    #> {chromote}   JS websocket 10:01:58.82 recv WEBSOCKET_MSG
    #> {chromote}   JS info      10:01:58.82 shinytest2; shiny:value distPlot
    #> {chromote}   JS info      10:01:59.01 shinytest2; Shiny has been idle for 200ms
    #> {shinytest2} R  info      10:01:59.01 Shiny app started
    #> {shiny}      R  stderr    ----------- Loading required package: shiny
    #> {shiny}      R  stderr    ----------- Running application in test mode.
    #> {shiny}      R  stderr    -----------
    #> {shiny}      R  stderr    ----------- Listening on http://127.0.0.1:4560
    #> {shiny}      R  stderr    ----------- SEND {"config":{"workerId":"","sessionId"|truncated
    #> {shiny}      R  stderr    ----------- RECV {"method":"init","data":{"bins":30,|truncated
    #> {shiny}      R  stderr    ----------- SEND {"custom":{"showcase-src":{"srcref":|truncated
    #> {shiny}      R  stderr    ----------- SEND {"busy":"busy"}
    #> {shiny}      R  stderr    ----------- SEND {"custom":{"showcase-src":{"srcref":|truncated
    #> {shiny}      R  stderr    ----------- SEND {"recalculating":{"name":"distPlot",|truncated
    #> {shiny}      R  stderr    ----------- SEND {"recalculating":{"name":"distPlot",|truncated
    #> {shiny}      R  stderr    ----------- SEND {"busy":"idle"}
    #> {shiny}      R  stderr    ----------- SEND {"errors":{},"values":{"distPlot":|truncated

    # The log that is returned is a `data.frame()`.
    log <- app2$get_logs()
    tibble::glimpse(log)
    #> Rows: 43
    #> Columns: 5
    #> $ workerid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    #> $ timestamp <dttm> 2022-09-19 10:01:57, 2022-09-19 10:01:57, 2022-09-19 10:01:58, 2022…
    #> $ location  <chr> "shinytest2", "shinytest2", "shinytest2", "shinytest2", "shinytest2"…
    #> $ level     <chr> "info", "info", "info", "info", "info", "info", "info", "info", "inf…
    #> $ message   <chr> "Start AppDriver initialization", "Starting Shiny app", "Creating ne…
    #> $ workerid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    #> $ timestamp <dttm> 2022-03-16 11:09:57, 2022-03-16 11:09:57, 2022-03-16 11:09:…
    #> $ location  <chr> "shinytest2", "shinytest2", "shinytest2", "shinytest2", "shi…
    #> $ level     <chr> "info", "info", "info", "info", "info", "info", "info", "inf…
    #> $ message   <chr> "Start AppDriver initialization", "Starting Shiny app", "Cre…

    # It may be filtered to find desired logs
    subset(log, level == "websocket")
    ## (All WebSocket messages have been replaced with `WEBSOCKET_MSG` in example below)
    #> {chromote} JS websocket 10:01:58.64 send WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.67 recv WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.71 recv WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.72 recv WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #> {chromote} JS websocket 10:01:58.82 recv WEBSOCKET_MSG
    }

------------------------------------------------------------------------

### Method `log_message()`

Add a message to the shinytest2 log.

#### Usage

    AppDriver$log_message(message)

#### Arguments

- `message`:

  Single message to store in log

#### Examples

    \dontrun{
    app_path <- system.file("examples/01_hello", package = "shiny")
    app <- AppDriver$new(app_path)

    app$log_message("Setting bins to smaller value")
    app$set_inputs(bins = 10)
    app$get_logs()
    }

------------------------------------------------------------------------

### Method [`stop()`](https://rdrr.io/r/base/stop.html)

Stop the Shiny application driver

This method stops all known processes:

- The Shiny application in the background R process,

- the background R process hosting the Shiny application, and

- the Chromote Session instance.

To stop your shiny application and return a value from `$stop()`, see
[`shiny::stopApp()`](https://rdrr.io/pkg/shiny/man/stopApp.html). This
is useful in testing to return context information.

Typically, this can be paired with a button that when clicked will call
`shiny::stopApp(info)` to return `info` from the test app back to the
main R session.

#### Usage

    AppDriver$stop(signal_timeout = missing_arg())

#### Arguments

- `signal_timeout`:

  Milliseconds to wait between sending a `SIGINT`, `SIGTERM`, and
  `SIGKILL` to the Shiny process. Defaults to 500ms and does not utilize
  the resolved value from `AppDriver$new(timeout=)`. However, if covr is
  currently executing, then the `timeout` is set to 20,000ms to allow
  for the coverage report to be generated.

#### Returns

The result of the background process if the Shiny application has
already been terminated.

#### Examples

    \dontrun{
    rlang::check_installed("reactlog")

    library(shiny)
    shiny_app <- shinyApp(
      ui = fluidPage(
        actionButton("button", "Stop app and return Reactlog"),
        "Click count:", textOutput("count")
      ),
      server = function(input, output) {
        output$count <- renderText({ input$button })
        observe({
          req(input$button)
          stopApp(shiny::reactlog())
        })
      }
    )

    app <- AppDriver$new(
      shiny_app,
      # Enable reactlog in background R session
      options = list(shiny.reactlog = TRUE)
    )

    app$click("button")
    rlog <- app$stop()
    str(head(rlog, 2))
    #> List of 2
    #> $ :List of 7
    #> ..$ action : chr "define"
    #> ..$ reactId: chr "r3"
    #> ..$ label  : chr "Theme Counter"
    #> ..$ type   : chr "reactiveVal"
    #> ..$ value  : chr " num 0"
    #> ..$ session: chr "bdc7417f2fc8c84fc05c9518e36fdc44"
    #> ..$ time   : num 1.65e+09
    #> $ :List of 7
    #> ..$ action : chr "define"
    #> ..$ reactId: chr "r4"
    #> ..$ label  : chr "output$count"
    #> .. ..- attr(*, "srcref")= int [1:6] 7 32 7 45 32 45
    #> .. ..- attr(*, "srcfile")= chr ""
    #> ..$ type   : chr "observer"
    #> ..$ value  : chr " NULL"
    #> ..$ session: chr "bdc7417f2fc8c84fc05c9518e36fdc44"
    #> ..$ time   : num 1.65e+09
    }

## Examples

``` r
## ------------------------------------------------
## Method `AppDriver$new`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Create an AppDriver from the Shiny app in the current directory
app <- AppDriver()

# Create an AppDriver object from a different Shiny app directory
example_app <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver(example_app)

# Expect consistent inital values
app$expect_values()
} # }

## ------------------------------------------------
## Method `AppDriver$view`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Open app in Chrome
app$view()
} # }

## ------------------------------------------------
## Method `AppDriver$click`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/07_widgets", package = "shiny")
app <- AppDriver$new(app_path)

tmpfile <- write.csv(cars, "cars.csv")
app$upload_file(file1 = tmpfile)
cat(app$get_text("#view"))
app$set_inputs(dataset = "cars", obs = 6)
app$click("update")
cat(app$get_text("#view"))
} # }

## ------------------------------------------------
## Method `AppDriver$set_inputs`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/07_widgets", package = "shiny")
app <- AppDriver$new(app_path)

cat(app$get_text("#view"))
app$set_inputs(dataset = "cars", obs = 6)
app$click("update")
cat(app$get_text("#view"))
} # }

## ------------------------------------------------
## Method `AppDriver$upload_file`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/09_upload", package = "shiny")
app <- AppDriver$new(app_path)

# Save example file
tmpfile <- tempfile(fileext = ".csv")
write.csv(cars, tmpfile, row.names = FALSE)

# Upload file to input named: file1
app$upload_file(file1 = tmpfile)
} # }

## ------------------------------------------------
## Method `AppDriver$expect_values`
## ------------------------------------------------

if (FALSE) { # \dontrun{
library(shiny)
shiny_app <- shinyApp(
  fluidPage(
    h1("Pythagorean theorem"),
    numericInput("A", "A", 3),
    numericInput("B", "B", 4),
    verbatimTextOutput("C"),
  ),
  function(input, output) {
    a_squared <- reactive({ req(input$A); input$A * input$A })
    b_squared <- reactive({ req(input$B); input$B * input$B })
    c_squared <- reactive({ a_squared() + b_squared() })
    c_value <- reactive({ sqrt(c_squared()) })
    output$C <- renderText({ c_value() })

    exportTestValues(
      a_squared = { a_squared() },
      b_squared = { b_squared() },
      c_squared = { c_squared() }
    )
  }
)

app <- AppDriver$new(shiny_app)

# Snapshot all known values
app$expect_values()

# Snapshot only `export` values
app$expect_values(export = TRUE)

# Snapshot values `"A"` from `input` and `"C"` from `output`
app$expect_values(input = "A", output = "C")
} # }

## ------------------------------------------------
## Method `AppDriver$get_value`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/04_mpg", package = "shiny")
app <- AppDriver$new(app_path)

# Retrieve a single value
app$get_value(output = "caption")
#> [1] "mpg ~ cyl"
# Equivalent code using `$get_values()`
app$get_values(output = "caption")$output$caption
#> [1] "mpg ~ cyl"
} # }

## ------------------------------------------------
## Method `AppDriver$get_values`
## ------------------------------------------------

if (FALSE) { # \dontrun{
library(shiny)
shiny_app <- shinyApp(
  fluidPage(
    h1("Pythagorean theorem"),
    numericInput("A", "A", 3),
    numericInput("B", "B", 4),
    verbatimTextOutput("C"),
  ),
  function(input, output) {
    a_squared <- reactive({ req(input$A); input$A * input$A })
    b_squared <- reactive({ req(input$B); input$B * input$B })
    c_squared <- reactive({ a_squared() + b_squared() })
    c_value <- reactive({ sqrt(c_squared()) })
    output$C <- renderText({ c_value() })

    exportTestValues(
      a_squared = { a_squared() },
      b_squared = { b_squared() },
      c_squared = { c_squared() }
    )
  }
)

app <- AppDriver$new(shiny_app)

# Show all known values
str(app$get_values())
#> List of 3
#> $ input :List of 2
#> ..$ A: int 3
#> ..$ B: int 4
#> $ output:List of 1
#> ..$ C: chr "5"
#> $ export:List of 3
#> ..$ a_squared: int 9
#> ..$ b_squared: int 16
#> ..$ c_squared: int 25

# Get only `export` values
str(app$get_values(export = TRUE))
#> List of 1
#> $ export:List of 3
#> ..$ a_squared: int 9
#> ..$ b_squared: int 16
#> ..$ c_squared: int 25

# Get values `"A"` from `input` and `"C"` from `output`
str(app$get_values(input = "A", output = "C"))
#> List of 2
#> $ input :List of 1
#> ..$ A: int 3
#> $ output:List of 1
#> ..$ C: chr "5"
} # }

## ------------------------------------------------
## Method `AppDriver$expect_download`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/10_download", package = "shiny")
app <- AppDriver$new(app_path)

# Save snapshot of rock.csv as 001.download
# Save snapshot value of `rock.csv` to capture default file name
app$expect_download("downloadData", compare = testthat::compare_file_text)
} # }

## ------------------------------------------------
## Method `AppDriver$get_download`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/10_download", package = "shiny")
app <- AppDriver$new(app_path)

# Get rock.csv as a tempfile
app$get_download("downloadData")
#> [1] "/TEMP/PATH/rock.csv"

# Get rock.csv as a "./myfile.csv"
app$get_download("downloadData", filename = "./myfile.csv")
#> [1] "./myfile.csv"
} # }

## ------------------------------------------------
## Method `AppDriver$expect_text`
## ------------------------------------------------

if (FALSE) { # \dontrun{
hello_app <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(hello_app)

# Make a snapshot of `"Hello Shiny!"`
app$expect_text("h2")
} # }

## ------------------------------------------------
## Method `AppDriver$get_text`
## ------------------------------------------------

if (FALSE) { # \dontrun{
hello_app <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(hello_app)

app$get_text("h2")
#> [1] "Hello Shiny!"
} # }

## ------------------------------------------------
## Method `AppDriver$expect_html`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/04_mpg", package = "shiny")
app <- AppDriver$new(app_path)

# Save a snapshot of the `caption` output
app$expect_html("#caption")
} # }

## ------------------------------------------------
## Method `AppDriver$get_html`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/03_reactivity", package = "shiny")
app <- AppDriver$new(app_path, check_names = FALSE)

app$set_inputs(caption = "Custom value!")
cat(app$get_html(".shiny-input-container")[1])
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="caption-label" for="caption">Caption:</label>
#>   <input id="caption" type="text" class="form-control shiny-bound-input" value="Data Summary">
#> </div>
## ^^ No update to the DOM of `caption`
} # }

## ------------------------------------------------
## Method `AppDriver$expect_js`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/07_widgets", package = "shiny")
app <- AppDriver$new(app_path)

# Track how many clicks are given to `#update` button
app$run_js("
  window.test_counter = 0;
  $('#update').click(() => window.test_counter++);
")
app$set_inputs(obs = 20)
# Click the update button, incrementing the counter
app$click("update")
# Save a snapshot of number of clicks (1)
app$expect_js("window.test_counter;")
} # }

## ------------------------------------------------
## Method `AppDriver$get_js`
## ------------------------------------------------

if (FALSE) { # \dontrun{
library(shiny)
shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
app <- AppDriver$new(shiny_app)

# Execute JavaScript code in the app's browser
app$get_js("1 + 1;")
#> [1] 2

# Execute a JavaScript Promise. Return the resolved value.
app$get_js("
  new Promise((resolve) => {
    setTimeout(() => resolve(1 + 1), 1000)
  }).
  then((value) => value + 1);
")
#> [1] 3

# With escaped arguments
loc_field <- "hostname"
js_txt <- paste0("window.location[", jsonlite::toJSON(loc_field, auto_unbox = TRUE), "]")
app$get_js(js_txt)
#> [1] "127.0.0.1"

# With `glue::glue()`
js_txt <- glue::glue_data(
  lapply(
    list(x = 40, y = 2),
    jsonlite::toJSON,
    auto_unbox = TRUE
  ),
  .open = "<", .close = ">",
  "let answer = function(a, b) {\n",
  "  return a + b;\n",
  "};\n",
  "answer(<x>, <y>);\n"
)
app$get_js(js_txt)
#> [1] 42
} # }

## ------------------------------------------------
## Method `AppDriver$run_js`
## ------------------------------------------------

if (FALSE) { # \dontrun{
library(shiny)
shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
app <- AppDriver$new(shiny_app)

# Get JavaScript answer from the app's browser
app$get_js("1 + 1")
#> [1] 2
# Execute JavaScript code in the app's browser
app$run_js("1 + 1")
# (Returns `app` invisibly)

# With escaped arguments
loc_field <- "hostname"
js_txt <- paste0("window.location[", jsonlite::toJSON(loc_field, auto_unbox = TRUE), "]")
app$run_js(js_txt)
app$get_js(js_txt)
#> [1] "127.0.0.1"
} # }

## ------------------------------------------------
## Method `AppDriver$expect_screenshot`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# These example lines should be performed in a `./tests/testthat`
# test file so that snapshot files can be properly saved

app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path, variant = platform_variant())

# Expect a full size screenshot to be pixel perfect
app$expect_screenshot()

# Images are brittle when containing plots
app$expect_screenshot(selector = "#distPlot")

# Test with more threshold in pixel value differences
# Helps with rounded corners
app$expect_screenshot(threshold = 10)

# Equivalent expectations
app$expect_screenshot() # default
app$expect_screenshot(threshold = NULL)
app$expect_screenshot(compare = testthat::compare_file_binary)
expect_snapshot_file(
  app$get_screenshot(),
  variant = app$get_variant(),
  compare = testthat::compare_file_binary
)

# Equivalent expectations
app$expect_screenshot(threshold = 3, kernel_size = 5)
app$expect_screenshot(compare = function(old, new) {
  compare_screenshot_threshold(
    old, new,
    threshold = 3,
    kernel_size = 5
  )
})
expect_screenshot_file(
  app$get_screenshot(),
  variant = app$get_variant(),
  compare = function(old, new) {
    compare_screenshot_threshold(
      old, new,
      threshold = 3,
      kernel_size = 5
    )
  }
)

# Take a screenshot of the entire scrollable area
app$expect_screenshot()
app$expect_screenshot(selector = "scrollable_area")

## Take a screenshot of the current viewport
# Shrink the window to be smaller than the app
app$set_window_size(400, 500)
# Scroll the viewport just a bit
app$run_js("window.scroll(30, 70)")
# Take screenshot of browser viewport
app$expect_screenshot(selector = "viewport")
} # }

## ------------------------------------------------
## Method `AppDriver$get_screenshot`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path)

# Display in graphics device
app$get_screenshot()

# Update bins then display `"disPlot"` in graphics device
app$set_inputs(bins = 10)
app$get_screenshot(selector = "#distPlot")

# Save screenshot to file and view it
tmpfile <- tempfile(fileext = ".png")
app$get_screenshot(tmpfile)
showimage::show_image(tmpfile)
} # }

## ------------------------------------------------
## Method `AppDriver$wait_for_idle`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path)

pre_value <- app$get_value(output = "distPlot")
# Update bins value
app$set_inputs(bins = 10, wait_ = FALSE)
middle_value <- app$get_value(output = "distPlot")
app$wait_for_idle()
post_value <- app$get_value(output = "distPlot")

# No guarantee that these values are different
identical(pre_value, middle_value)
# Will not be equal
identical(pre_value, post_value)

# ---------------------
## Change the screen size to trigger a plot update
pre_value <- app$get_value(output = "distPlot")
app$set_window_size(height = 1080, width = 1920, wait = FALSE)
middle_value <- app$get_value(output = "distPlot")
app$wait_for_idle()
post_value <- app$get_value(output = "distPlot")

# No guarantee that these values are different
identical(pre_value, middle_value)
# Will not be equal
identical(pre_value, post_value)
} # }

## ------------------------------------------------
## Method `AppDriver$wait_for_value`
## ------------------------------------------------

if (FALSE) { # \dontrun{
library(shiny)
shiny_app <- shinyApp(
  fluidPage(
    h1("Dynamic output"),
    actionButton("display", "Display UI"),
    uiOutput("dym1"),
  ),
  function(input, output) {
    output$dym1 <- renderUI({
      req(input$display)
      Sys.sleep(runif(1, max = 2)) # Artificial calculations
      tagList(
        sliderInput("slider1", "Slider #1", 0, 100, 25),
        uiOutput("dym2")
      )
    })
    output$dym2 <- renderUI({
      Sys.sleep(runif(1, max = 2)) # Artificial calculations
      tagList(
        sliderInput("slider2", "Slider #2", 0, 100, 50),
        "Total:", verbatimTextOutput("total")
      )
    })
    output$total <- renderText({
      req(input$slider1, input$slider2)
      input$slider1 + input$slider2
    })
  }
)

app <- AppDriver$new(shiny_app)

# Create UI / output values
app$click("display")
# Wait for total to be calculated (or have a non-NULL value)
new_total_value <- app$wait_for_value(output = "total")
#> [1] "75"
app$get_value(output = "total")
#> [1] "75"
} # }

## ------------------------------------------------
## Method `AppDriver$wait_for_js`
## ------------------------------------------------

if (FALSE) { # \dontrun{
shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
app <- AppDriver$new(shiny_app)

# Contrived example:
# Wait until `Date.now()` returns a number that ends in a 5. (0 - 10 seconds)
system.time(
  app$wait_for_js("Math.floor((Date.now() / 1000) % 10) == 5;")
)

## A second example where we run the contents of a JavaScript file
## and use the result to wait for a condition
app$run_js(file = "complicated_file.js")
app$wait_for_js("complicated_condition();")
} # }

## ------------------------------------------------
## Method `AppDriver$expect_unique_names`
## ------------------------------------------------

if (FALSE) { # \dontrun{
shiny_app <- shinyApp(
  ui = fluidPage(
    # Duplicate input IDs: `"text"`
    textInput("text", "Text 1"),
    textInput("text", "Text 2")
  ),
  server = function(input, output) {
    # empty
  }
)
# Initial checking for unique names (default behavior)
app <- AppDriver$new(shiny_app, check_names = TRUE)
#> Warning:
#> ! Shiny inputs should have unique HTML id values.
#> i The following HTML id values are not unique:
#> • text
app$stop()

# Manually assert that all names are unique
app <- AppDriver$new(shiny_app, check_names = FALSE)
app$expect_unique_names()
#> Error: `app_check_unique_names(self, private)` threw an unexpected warning.
#> Message: ! Shiny inputs should have unique HTML id values.
#> i The following HTML id values are not unique:
#>   • text
#> Class:   rlang_warning/warning/condition
app$stop()
} # }

## ------------------------------------------------
## Method `AppDriver$get_dir`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path)

identical(app$get_dir(), app_path)
#> [1] TRUE
} # }

## ------------------------------------------------
## Method `AppDriver$get_url`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path)

browseURL(app$get_url())
} # }

## ------------------------------------------------
## Method `AppDriver$get_window_size`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path)

app$get_window_size()
#> $width
#> [1] 992
#>
#> $height
#> [1] 1323
} # }

## ------------------------------------------------
## Method `AppDriver$set_window_size`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
# Set init window size
app <- AppDriver$new(app_path, height = 1400, width = 1000)

app$get_window_size()
#> $width
#> [1] 1000
#>
#> $height
#> [1] 1400

# Manually set the window size
app$set_window_size(height = 1080, width = 1920)
app$get_window_size()
#> $width
#> [1] 1920
#>
#> $height
#> [1] 1080
} # }

## ------------------------------------------------
## Method `AppDriver$get_chromote_session`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path)

b <- app$get_chromote_session()
b$Runtime$evaluate("1 + 1")
#> $result
#> $result$type
#> [1] "number"
#>
#> $result$value
#> [1] 2
#>
#> $result$description
#> [1] "2"
} # }

## ------------------------------------------------
## Method `AppDriver$get_variant`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")

app <- AppDriver$new(app_path)

app$get_variant()
#> NULL

app <- AppDriver$new(app_path, variant = platform_variant())
app$get_variant()
#> [1] "mac-4.1"
} # }

## ------------------------------------------------
## Method `AppDriver$get_logs`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app1 <- AppDriver$new(system.file("examples/01_hello", package = "shiny"))

app1$get_logs()
#> {shinytest2} R  info   10:00:28.86 Start AppDriver initialization
#> {shinytest2} R  info   10:00:28.86 Starting Shiny app
#> {shinytest2} R  info   10:00:29.76 Creating new ChromoteSession
#> {shinytest2} R  info   10:00:30.56 Navigating to Shiny app
#> {shinytest2} R  info   10:00:30.70 Injecting shiny-tracer.js
#> {chromote}   JS info   10:00:30.75 shinytest2; jQuery found
#> {chromote}   JS info   10:00:30.77 shinytest2; Waiting for shiny session to connect
#> {chromote}   JS info   10:00:30.77 shinytest2; Loaded
#> {shinytest2} R  info   10:00:30.77 Waiting for Shiny to become ready
#> {chromote}   JS info   10:00:30.90 shinytest2; Connected
#> {chromote}   JS info   10:00:30.95 shinytest2; shiny:busy
#> {shinytest2} R  info   10:00:30.98 Waiting for Shiny to become idle for 200ms within 15000ms
#> {chromote}   JS info   10:00:30.98 shinytest2; Waiting for Shiny to be stable
#> {chromote}   JS info   10:00:31.37 shinytest2; shiny:idle
#> {chromote}   JS info   10:00:31.38 shinytest2; shiny:value distPlot
#> {chromote}   JS info   10:00:31.57 shinytest2; Shiny has been idle for 200ms
#> {shinytest2} R  info   10:00:31.57 Shiny app started
#> {shiny}      R  stderr ----------- Loading required package: shiny
#> {shiny}      R  stderr ----------- Running application in test mode.
#> {shiny}      R  stderr -----------
#> {shiny}      R  stderr ----------- Listening on http://127.0.0.1:4679


# To capture all websocket traffic, set `options = list(shiny.trace = TRUE)`
app2 <- AppDriver$new(
  system.file("examples/01_hello", package = "shiny"),
  options = list(shiny.trace = TRUE)
)

app2$get_logs()
## (All WebSocket messages have been replaced with `WEBSOCKET_MSG` in example below)
#> {shinytest2} R  info      10:01:57.49 Start AppDriver initialization
#> {shinytest2} R  info      10:01:57.50 Starting Shiny app
#> {shinytest2} R  info      10:01:58.20 Creating new ChromoteSession
#> {shinytest2} R  info      10:01:58.35 Navigating to Shiny app
#> {shinytest2} R  info      10:01:58.47 Injecting shiny-tracer.js
#> {chromote}   JS info      10:01:58.49 shinytest2; jQuery not found
#> {chromote}   JS info      10:01:58.49 shinytest2; Loaded
#> {shinytest2} R  info      10:01:58.50 Waiting for Shiny to become ready
#> {chromote}   JS info      10:01:58.55 shinytest2; jQuery found
#> {chromote}   JS info      10:01:58.55 shinytest2; Waiting for shiny session to connect
#> {chromote}   JS websocket 10:01:58.64 send WEBSOCKET_MSG
#> {chromote}   JS websocket 10:01:58.67 recv WEBSOCKET_MSG
#> {chromote}   JS info      10:01:58.67 shinytest2; Connected
#> {chromote}   JS websocket 10:01:58.71 recv WEBSOCKET_MSG
#> {chromote}   JS websocket 10:01:58.72 recv WEBSOCKET_MSG
#> {chromote}   JS info      10:01:58.72 shinytest2; shiny:busy
#> {chromote}   JS websocket 10:01:58.73 recv WEBSOCKET_MSG
#> {chromote}   JS websocket 10:01:58.73 recv WEBSOCKET_MSG
#> {shinytest2} R  info      10:01:58.75 Waiting for Shiny to become idle for 200ms within 15000ms
#> {chromote}   JS info      10:01:58.75 shinytest2; Waiting for Shiny to be stable
#> {chromote}   JS websocket 10:01:58.81 recv WEBSOCKET_MSG
#> {chromote}   JS websocket 10:01:58.81 recv WEBSOCKET_MSG
#> {chromote}   JS info      10:01:58.81 shinytest2; shiny:idle
#> {chromote}   JS websocket 10:01:58.82 recv WEBSOCKET_MSG
#> {chromote}   JS info      10:01:58.82 shinytest2; shiny:value distPlot
#> {chromote}   JS info      10:01:59.01 shinytest2; Shiny has been idle for 200ms
#> {shinytest2} R  info      10:01:59.01 Shiny app started
#> {shiny}      R  stderr    ----------- Loading required package: shiny
#> {shiny}      R  stderr    ----------- Running application in test mode.
#> {shiny}      R  stderr    -----------
#> {shiny}      R  stderr    ----------- Listening on http://127.0.0.1:4560
#> {shiny}      R  stderr    ----------- SEND {"config":{"workerId":"","sessionId"|truncated
#> {shiny}      R  stderr    ----------- RECV {"method":"init","data":{"bins":30,|truncated
#> {shiny}      R  stderr    ----------- SEND {"custom":{"showcase-src":{"srcref":|truncated
#> {shiny}      R  stderr    ----------- SEND {"busy":"busy"}
#> {shiny}      R  stderr    ----------- SEND {"custom":{"showcase-src":{"srcref":|truncated
#> {shiny}      R  stderr    ----------- SEND {"recalculating":{"name":"distPlot",|truncated
#> {shiny}      R  stderr    ----------- SEND {"recalculating":{"name":"distPlot",|truncated
#> {shiny}      R  stderr    ----------- SEND {"busy":"idle"}
#> {shiny}      R  stderr    ----------- SEND {"errors":{},"values":{"distPlot":|truncated

# The log that is returned is a `data.frame()`.
log <- app2$get_logs()
tibble::glimpse(log)
#> Rows: 43
#> Columns: 5
#> $ workerid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ timestamp <dttm> 2022-09-19 10:01:57, 2022-09-19 10:01:57, 2022-09-19 10:01:58, 2022…
#> $ location  <chr> "shinytest2", "shinytest2", "shinytest2", "shinytest2", "shinytest2"…
#> $ level     <chr> "info", "info", "info", "info", "info", "info", "info", "info", "inf…
#> $ message   <chr> "Start AppDriver initialization", "Starting Shiny app", "Creating ne…
#> $ workerid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ timestamp <dttm> 2022-03-16 11:09:57, 2022-03-16 11:09:57, 2022-03-16 11:09:…
#> $ location  <chr> "shinytest2", "shinytest2", "shinytest2", "shinytest2", "shi…
#> $ level     <chr> "info", "info", "info", "info", "info", "info", "info", "inf…
#> $ message   <chr> "Start AppDriver initialization", "Starting Shiny app", "Cre…

# It may be filtered to find desired logs
subset(log, level == "websocket")
## (All WebSocket messages have been replaced with `WEBSOCKET_MSG` in example below)
#> {chromote} JS websocket 10:01:58.64 send WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.67 recv WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.71 recv WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.72 recv WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.73 recv WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.73 recv WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.81 recv WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.81 recv WEBSOCKET_MSG
#> {chromote} JS websocket 10:01:58.82 recv WEBSOCKET_MSG
} # }

## ------------------------------------------------
## Method `AppDriver$log_message`
## ------------------------------------------------

if (FALSE) { # \dontrun{
app_path <- system.file("examples/01_hello", package = "shiny")
app <- AppDriver$new(app_path)

app$log_message("Setting bins to smaller value")
app$set_inputs(bins = 10)
app$get_logs()
} # }

## ------------------------------------------------
## Method `AppDriver$stop`
## ------------------------------------------------

if (FALSE) { # \dontrun{
rlang::check_installed("reactlog")

library(shiny)
shiny_app <- shinyApp(
  ui = fluidPage(
    actionButton("button", "Stop app and return Reactlog"),
    "Click count:", textOutput("count")
  ),
  server = function(input, output) {
    output$count <- renderText({ input$button })
    observe({
      req(input$button)
      stopApp(shiny::reactlog())
    })
  }
)

app <- AppDriver$new(
  shiny_app,
  # Enable reactlog in background R session
  options = list(shiny.reactlog = TRUE)
)

app$click("button")
rlog <- app$stop()
str(head(rlog, 2))
#> List of 2
#> $ :List of 7
#> ..$ action : chr "define"
#> ..$ reactId: chr "r3"
#> ..$ label  : chr "Theme Counter"
#> ..$ type   : chr "reactiveVal"
#> ..$ value  : chr " num 0"
#> ..$ session: chr "bdc7417f2fc8c84fc05c9518e36fdc44"
#> ..$ time   : num 1.65e+09
#> $ :List of 7
#> ..$ action : chr "define"
#> ..$ reactId: chr "r4"
#> ..$ label  : chr "output$count"
#> .. ..- attr(*, "srcref")= int [1:6] 7 32 7 45 32 45
#> .. ..- attr(*, "srcfile")= chr ""
#> ..$ type   : chr "observer"
#> ..$ value  : chr " NULL"
#> ..$ session: chr "bdc7417f2fc8c84fc05c9518e36fdc44"
#> ..$ time   : num 1.65e+09
} # }
```
