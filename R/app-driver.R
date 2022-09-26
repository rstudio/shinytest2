#' @importFrom rlang missing_arg
#' @importFrom callr process
NULL

#' Drive a Shiny application
#'
#' @description
#' This class starts a Shiny app in a new R session, along with \pkg{chromote}'s
#' headless browser that can be used to simulate user actions. This provides
#' a full simulation of a Shiny app so that you can test user interactions
#' with a live app.
#'
#' Methods described below are ordered by perceived popularity.
#' _Expect_ methods are grouped next to their corresponding _get_ methods.
#'
#' @section Vignettes:
#'
#' Please see [Testing in depth](https://rstudio.github.io/shinytest2/articles/in-depth.html)
#' for more details about the different expectation methods.
#'
#' Please see [Robust testing](https://rstudio.github.io/shinytest2/articles/robust.html)
#' for more details about the cost / benefits for each expectation method.
#'
#' @section Test mode:
#'
#' To have your `AppDriver` retrieve values from your Shiny app, be sure to
#' set `shiny::runApp(test.mode = TRUE)` when running your Shiny app.
#'
#' If you are deploying your Shiny app where you do not have control over
#' the call to `shiny::runApp()`, you can set `options(shiny.testmode = TRUE)` in
#' a `.Rprofile` file within your Shiny app directory.
#'
#'
#' @section Start-up failure:
#'
#' If the app throws an error during initialization, the `AppDriver` will
#' be stored in `rlang::last_error()$app`. This allows for the "failure
#' to initialize" to be signaled while also allowing for the `app` to be
#' retrieved after any initialization error has been thrown.
#'
#' @section Exporting reactive values:
#'
#' Reactive values from within your Shiny application can be exported using the
#' method:
#' [`shiny::exportTestValues()`](https://shiny.rstudio.com/reference/shiny/latest/exportTestValues.html).
#' This underutilized method exposes internal values of your app
#' without needing to create a corresponding input value or output value.
#'
#' For example:
#'
#' ```r
#' library(shiny)
#' shiny_app <- shinyApp(
#'   fluidPage(
#'     h1("Pythagorean theorem"),
#'     numericInput("A", "A", 3),
#'     numericInput("B", "B", 4),
#'     verbatimTextOutput("C"),
#'   ),
#'   function(input, output) {
#'     a_squared <- reactive({ req(input$A); input$A * input$A })
#'     b_squared <- reactive({ req(input$B); input$B * input$B })
#'     c_squared <- reactive({ a_squared() + b_squared() })
#'     c_value <- reactive({ sqrt(c_squared()) })
#'     output$C <- renderText({ c_value() })
#'
#'     exportTestValues(
#'       a_squared = { a_squared() },
#'       b_squared = { b_squared() },
#'       c_squared = { c_squared() }
#'     )
#'   }
#' )
#'
#' app <- AppDriver$new(shiny_app)
#'
#' init_vals <- app$get_values()
#' str(init_vals)
#' #> List of 3
#' #> $ input :List of 2
#' #> ..$ A: int 3
#' #> ..$ B: int 4
#' #> $ output:List of 1
#' #> ..$ C: chr "5"
#' #> $ export:List of 3
#' #> ..$ a_squared: int 9
#' #> ..$ b_squared: int 16
#' #> ..$ c_squared: int 25
#' ```
#'
#' These exported test values are only exposed when `shiny::runApp(test.mode = TRUE)`
#' is set.  \pkg{shinytest2} sets this variable when running Shiny based app or
#' document.
#'
#' @section \pkg{testthat} wrappers:
#'
#' The two main expectation methods: `$expect_values()` and `$expect_screenshot()`
#' eventually wrap [`testthat::expect_snapshot_file()`].
#'
#' Their underlying logic is similar to:
#' ```r
#' ## Expect values
#' tmpfile <- tempfile(fileext = ".json")
#' jsonlite::write_json(app$get_values(), tmpfile)
#' expect_snapshot_file(
#'   tmpfile,
#'   variant = app$get_variant(),
#'   compare = testthat::compare_file_text,
#'   cran = cran
#' )
#'
#'
#' ## Expect screenshot
#' tmpfile <- tempfile(fileext = ".png")
#' app$get_screenshot(tmpfile)
#' expect_snapshot_file(
#'   tmpfile,
#'   variant = app$get_variant(),
#'   compare = testthat::compare_file_binary,
#'   cran = cran
#' )
#' ```
#'
#' To update the snapshot values, you will need to run a variation of
#' [`testthat::snapshot_review()`].
#'
#'
#' @param ... Must be empty. Allows for parameter expansion.
#' @param timeout Amount of time to wait before giving up (milliseconds).
#'   Defaults to the resolved `timeout` value during the `AppDriver` initialization.
#' @param timeout_ Amount of time to wait before giving up (milliseconds).
#'   Defaults to the resolved `timeout` value during the `AppDriver` initialization.
#' @param cran Should these expectations be verified on CRAN? By default,
#'        they are not because snapshot tests tend to be fragile
#'        because they often rely on minor details of dependencies.
#' @param wait_ Wait until all reactive updates have completed?
#' @param hash_images If `TRUE`, images will be hashed before being returned.
#'   Otherwise, all images will return their full data64 encoded value.
#' @param screenshot_args This named list of arguments is passed along to
#'   [`chromote::ChromoteSession`]'s `$get_screenshot()` method. If missing, the
#'   value will default to `$new(screenshot_args=)`.
#'
#' If the value is:
#'   * `TRUE`: A screenshot of the whole page will be taken with no delay
#'   * A named list of arguments: Arguments passed directly to [`chromote::ChromoteSession`]'s
#' `$get_screenshot()` method. The `selector` and `delay` will default to `"html"` and `0` respectively.
#'
#' If a `FALSE` value is provided, the parameter will be ignored and a
#' screenshot will be taken with default behavior.
#' @param delay The number of **seconds** to wait before taking the screenshot.
#'   This value can be supplied as `delay` or `screenshot_args$delay`, with the
#'   `delay` parameter having preference.
#' @param selector The selector is a CSS selector that will be used to select a
#'   portion of the page to be captured. This value can be supplied as
#'   `selector` or `screenshot_args$selector`, with the `selector` parameter
#'   having preference.
#' @importFrom R6 R6Class
#' @seealso [`platform_variant()`], [`use_shinytest2_test()`]
#' @export
AppDriver <- R6Class( # nolint
  "AppDriver",
  cloneable = FALSE,
  private = list(
    chromote_session = "<chromote::ChromoteSession>",
    shiny_process = NULL, # `callr::r_bg()` object
    shiny_proc_value = NULL, # Output of `private$shiny_process$value()`

    counter = "<Count>",
    shiny_url = "<Url>",

    load_timeout = 15 * 1000, # Time for shiny app to load; ms
    timeout = 4 * 1000, # Default timeout value; ms

    logs = list(), # List of all log messages added via `$log_message()`

    clean_logs = TRUE, # Whether to clean logs when GC'd

    name = NULL, # character / NULL
    variant = "missing_value()", # character / NULL
    state = "stopped", # "stopped" or "running"
    shiny_worker_id = NA_character_,

    dir = NULL, # Full dir path to app. No file path included
    save_dir = NULL, # Temp folder to store snapshot outputs
    default_screenshot_args = "missing_value()", # Default screenshot args to use
    default_expect_values_screenshot_args = TRUE, # Should `$expect_values()` expect a non-failing screenshot?
    shiny_test_url = NULL, # URL for shiny's test API

    finalize = function() {
      app_finalize(self, private)
    }

  ),
  public = list(

    #' @description
    #' Initialize an `AppDriver` object
    #'
    #' @param app_dir This value can be many different things:
    #'   * A directory containing your Shiny application or a run-time Shiny R
    #'     Markdown document.
    #'   * A URL pointing to your shiny application. (Don't forget to set
    #'     `testmode = TRUE` when running your application!)
    #'   * A Shiny application object which inherits from `"shiny.appobj"`.
    #'
    #' By default, `app_dir` is set to `test_path("../../")` to work in both
    #' interactive and testing usage.
    #'
    #' If a file path is not provided to `app_dir`, snapshots will be saved as
    #' if the root of the Shiny application was the current directory.
    #'
    #' @param name Prefix value to use when saving testthat snapshot files. Ex:
    #'   `NAME-001.json`. Name **must** be unique when saving multiple snapshots
    #'   from within the same testing file. Otherwise, two different `AppDriver`
    #'   objects will be referencing the same files.
    #' @template variant
    #' @param seed An optional random seed to use before starting the application.
    #'   For apps that use R's random number generator, this can make their
    #'   behavior repeatable.
    #' @param load_timeout How long to wait for the app to load, in ms.
    #'   This includes the time to start R. Defaults to 15s.
    #'
    #'   If `load_timeout` is missing, the first numeric value found will be used:
    #'   * R option `options(shinytest2.load_timeout=)`
    #'   * System environment variable `SHINYTEST2_LOAD_TIMEOUT`
    #'   * `15 * 1000` (15 seconds)
    #' @param timeout Default number of milliseconds for any `timeout` or
    #'   `timeout_` parameter in the `AppDriver` class. Defaults to 4s.
    #'
    #'   If `timeout` is missing, the first numeric value found will be used:
    #'   * R option `options(shinytest2.timeout=)`
    #'   * System environment variable `SHINYTEST2_TIMEOUT`
    #'   * `4 * 1000` (4 seconds)
    #' @param wait If `TRUE`, `$wait_for_idle(duration = 200, timeout = load_timeout)`
    #'   will be called once the app has connected a new session, blocking until the
    #'   Shiny app is idle for 200ms.
    #' @param screenshot_args Default set of arguments to pass in to
    #'   [`chromote::ChromoteSession`]'s `$get_screenshot()` method when taking
    #'   screenshots within `$expect_screenshot()`. To disable screenshots by
    #'   default, set to `FALSE`.
    #' @param expect_values_screenshot_args The value for `screenshot_args` when
    #'   producing a debug screenshot for `$expect_values()`. To disable debug
    #'   screenshots by default, set to `FALSE`.
    #' @param check_names Check if widget names are unique once the application
    #'   initially loads? If duplicate names are found on initialization, a
    #'   warning will be displayed.
    #' @param view Opens the [`ChromoteSession`] in an interactive browser tab
    #'   before attempting to navigate to the Shiny app.
    #' @param height,width Window size to use when opening the
    #'   [`ChromoteSession`]. Both `height` and `width` values must be non-null
    #'   values to be used.
    #' @param clean_logs Whether to remove the `stdout` and `stderr` Shiny app
    #'   logs when the `AppDriver` object is garbage collected.
    #' @param shiny_args A list of options to pass to [shiny::runApp()]. Ex:
    #' `list(port = 8080)`.
    #' @param render_args Passed to `rmarkdown::run(render_args=)` for
    #' interactive `.Rmd`s. Ex: `list(quiet = TRUE)`
    #' @param options A list of [base::options()] to set in the Shiny
    #'   application's child R process. See [`shiny::shinyOptions()`] for
    #'   inspiration. If `shiny.trace = TRUE`, then all WebSocket traffic will
    #'   be captured by `chromote` and time-stamped for logging purposes.
    #' @return An object with class `AppDriver` and the many methods described in this documentation.
    #' @examples
    #' \dontrun{
    #' # Create an AppDriver from the Shiny app in the current directory
    #' app <- AppDriver()
    #'
    #' # Create an AppDriver object from a different Shiny app directory
    #' example_app <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver(example_app)
    #'
    #' # Expect consistent inital values
    #' app$expect_values()
    #' }
    initialize = function(
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
    ) {
      app_initialize(
        self, private,
        app_dir = app_dir,
        ...,
        load_timeout = load_timeout,
        timeout = timeout,
        wait = wait,
        expect_values_screenshot_args = expect_values_screenshot_args,
        screenshot_args = screenshot_args,
        check_names = check_names,
        name = name,
        variant = variant,
        view = view,
        height = height,
        width = width,
        seed = seed,
        clean_logs = clean_logs,
        shiny_args = shiny_args,
        render_args = render_args,
        options = options
      )
    },


    #' @description
    #' View the Shiny application
    #'
    #' Calls `$view()` on the [`ChromoteSession`] object to _view_ your Shiny
    #' application in a Chrome browser.
    #'
    #' This method is very helpful for debugging while writing your tests.
    #' @examples
    #' \dontrun{
    #' # Open app in Chrome
    #' app$view()
    #' }
    view = function() {
      app_view(self, private)
    },


    #' @description Click an element
    #'
    #' Find a Shiny input/output value or DOM CSS selector and click it using
    #' the [DOM method
    #' `TAG.click()`](https://www.w3schools.com/jsref/met_html_click.asp).
    #'
    #' This method can be used to click input buttons and other elements that
    #' need to simulate a click action.
    #'
    #' @param input,output,selector A name of an Shiny `input`/`output` value or
    #'   a DOM CSS selector. Only one of these may be used.
    #' @param ... If `input` is used, all extra arguments are passed to
    #'   `$set_inputs(!!input := "click", ...)`. This means that the
    #'   `AppDriver` will wait until an output has been updated within the
    #'   specified `timeout_`. When clicking any other content, `...` must be empty.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/07_widgets", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' tmpfile <- write.csv(cars, "cars.csv")
    #' app$upload_file(file1 = tmpfile)
    #' cat(app$get_text("#view"))
    #' app$set_inputs(dataset = "cars", obs = 6)
    #' app$click("update")
    #' cat(app$get_text("#view"))
    #' }
    click = function(
      input = missing_arg(), output = missing_arg(), selector = missing_arg(),
      ...
    ) {
      app_click(self, private, input = input, output = output, selector = selector, ...)
    },

    #' @description Set input values
    #'
    #' Set Shiny inputs by sending the value to the Chrome browser and
    #' programmatically updating the values. Given `wait_ = TRUE`, the method will
    #' not return until an output value has been updated.
    #'
    #' @param ... Name-value pairs, `component_name_1 = value_1, component_name_2 = value_2` etc.
    #'   Input with name `component_name_1` will be assigned value `value_1`.
    #' @param allow_no_input_binding_ When setting the value of an input, allow
    #'   it to set the value of an input even if that input does not have an
    #'   input binding. This is useful to replicate behavior like hovering over
    #'   a \pkg{plotly} plot.
    #' @param priority_ Sets the event priority. For expert use only: see
    #'   <https://shiny.rstudio.com/articles/communicating-with-js.html#values-vs-events> for details.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/07_widgets", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' cat(app$get_text("#view"))
    #' app$set_inputs(dataset = "cars", obs = 6)
    #' app$click("update")
    #' cat(app$get_text("#view"))
    #' }
    set_inputs = function(
      ...,
      wait_ = TRUE,
      timeout_ = missing_arg(),
      allow_no_input_binding_ = FALSE,
      priority_ = c("input", "event")
    ) {
      app_set_inputs(
        self, private, ..., wait_ = wait_, timeout_ = timeout_,
        allow_no_input_binding_ = allow_no_input_binding_, priority_ = priority_
      )
    },


    #' @description Upload a file
    #'
    #' Uploads a file to the specified file input.
    #' @param ... Name-path pair, e.g. `component_name = file_path`. The file located at
    #' `file_path` will be uploaded to file input with name `component_name`.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/09_upload", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Save example file
    #' tmpfile <- tempfile(fileext = ".csv")
    #' write.csv(cars, tmpfile, row.names = FALSE)
    #'
    #' # Upload file to input named: file1
    #' app$upload_file(file1 = tmpfile)
    #' }
    upload_file = function(..., wait_ = TRUE, timeout_ = missing_arg()) {
      app_upload_file(self, private, ..., wait_ = wait_, timeout_ = timeout_)
    },

    #' @description
    #' Expect `input`, `output`, and `export` values
    #'
    #' A JSON snapshot is saved of given the results from the underlying call to `$get_values()`.
    #'
    #' Note, values that contain environments or other values that will have
    #' trouble serializing may not work well. Instead, these objects should be
    #' manually inspected and have their components tested individually.
    #'
    #' Please see [Robust testing](https://rstudio.github.io/shinytest2/articles/robust.html) for more details.
    #'
    #' @param name The file name to be used for the snapshot. The file extension
    #'   will be overwritten to `.json`. By default, the `name` supplied to
    #'   `app` on initialization with a counter will be used (e.g. `"NAME-001.json"`).
    #' @param input,output,export
    #'   Depending on which parameters are supplied, different return values can occur:
    #'     * If `input`, `output`, and `export` are all missing, then all values are included in the snapshot.
    #'     * If at least one `input`, `output`, or `export` is specified, then only the requested values are included in the snapshot.
    #'
    #'   The values supplied to each variable can be:
    #'     * A character vector of specific names to only include in the snapshot.
    #'     * `TRUE` to request that all values of that type are included in the snapshot.
    #'     * Anything else (e.g. `NULL` or `FALSE`) will result in the parameter being ignored.
    #' @param screenshot_args This value is passed along to
    #'   `$expect_screenshot()` where the resulting snapshot expectation is ignored. If
    #'   missing, the default value will be
    #'   `$new(expect_values_screenshot_args=)`.
    #'
    #'   The final value can either be:
    #'   * `TRUE`: A screenshot of the whole page will be taken with no delay
    #'   * `FALSE`: No screenshot will be taken
    #'   * A named list of arguments. These arguments are passed directly to
    #'     [`chromote::ChromoteSession`]'s `$get_screenshot()` method. The `selector`
    #'     and `delay` will default to `"html"` and `0` respectively.
    #' @return The result of the snapshot expectation
    #' @examples
    #' \dontrun{
    #' library(shiny)
    #' shiny_app <- shinyApp(
    #'   fluidPage(
    #'     h1("Pythagorean theorem"),
    #'     numericInput("A", "A", 3),
    #'     numericInput("B", "B", 4),
    #'     verbatimTextOutput("C"),
    #'   ),
    #'   function(input, output) {
    #'     a_squared <- reactive({ req(input$A); input$A * input$A })
    #'     b_squared <- reactive({ req(input$B); input$B * input$B })
    #'     c_squared <- reactive({ a_squared() + b_squared() })
    #'     c_value <- reactive({ sqrt(c_squared()) })
    #'     output$C <- renderText({ c_value() })
    #'
    #'     exportTestValues(
    #'       a_squared = { a_squared() },
    #'       b_squared = { b_squared() },
    #'       c_squared = { c_squared() }
    #'     )
    #'   }
    #' )
    #'
    #' app <- AppDriver$new(shiny_app)
    #'
    #' # Snapshot all known values
    #' app$expect_values()
    #'
    #' # Snapshot only `export` values
    #' app$expect_values(export = TRUE)
    #'
    #' # Snapshot values `"A"` from `input` and `"C"` from `output`
    #' app$expect_values(input = "A", output = "C")
    #' }
    expect_values = function(
      ...,
      input = missing_arg(), output = missing_arg(), export = missing_arg(),
      screenshot_args = missing_arg(),
      name = NULL,
      cran = FALSE
      ) {
      app_expect_values(
        self, private,
        ...,
        input = input, output = output, export = export,
        screenshot_args = screenshot_args,
        name = name,
        cran = cran
      )
    },
    #' @description
    #' Get a single `input`, `output`, or `export` value
    #'
    #' This is a helper function around `$get_values()` to retrieve a single
    #' `input`, `output`, or `export` value. Only a single `input`, `output`, or
    #' `export` value can be used.
    #'
    #' Note, values that contain environments or other values that will have
    #' trouble serializing to RDS may not work well.
    #'
    #' @param input,output,export One of these variable should contain a single
    #'   string value. If more than one value is specified or no values are
    #'   specified, an error will be thrown.
    #' @return The requested `input`, `output`, or `export` value.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/04_mpg", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Retrieve a single value
    #' app$get_value(output = "caption")
    #' #> [1] "mpg ~ cyl"
    #' # Equivalent code using `$get_values()`
    #' app$get_values(output = "caption")$output$caption
    #' #> [1] "mpg ~ cyl"
    #' }
    get_value = function(
      ...,
      input = missing_arg(), output = missing_arg(), export = missing_arg(),
      hash_images = FALSE
    ) {
      app_get_value(
        self, private,
        ...,
        input = input, output = output, export = export,
        hash_images = hash_images
      )
    },
    #' @description
    #' Get `input`, `output`, and `export` values
    #'
    #' Retrieves a list of all known `input`, `output`, or `export` values. This
    #' method is a core method when inspecting your Shiny app.
    #'
    #' Note, values that contain environments or other values that will have
    #' trouble serializing may not work well.
    #'
    #' @param input,output,export
    #'   Depending on which parameters are supplied, different return values can occur:
    #'     * If `input`, `output`, and `export` are all missing, then all values are included in the snapshot.
    #'     * If at least one `input`, `output`, or `export` is specified, then only the requested values are included in the snapshot.
    #'
    #'   The values supplied to each variable can be:
    #'     * A character vector of specific names to only include in the snapshot.
    #'     * `TRUE` to request that all values of that type are included in the snapshot.
    #'     * Anything else (e.g. `NULL` or `FALSE`) will result in the parameter being ignored.
    #' @return A named list of all inputs, outputs, and export values.
    #' @examples
    #' \dontrun{
    #' library(shiny)
    #' shiny_app <- shinyApp(
    #'   fluidPage(
    #'     h1("Pythagorean theorem"),
    #'     numericInput("A", "A", 3),
    #'     numericInput("B", "B", 4),
    #'     verbatimTextOutput("C"),
    #'   ),
    #'   function(input, output) {
    #'     a_squared <- reactive({ req(input$A); input$A * input$A })
    #'     b_squared <- reactive({ req(input$B); input$B * input$B })
    #'     c_squared <- reactive({ a_squared() + b_squared() })
    #'     c_value <- reactive({ sqrt(c_squared()) })
    #'     output$C <- renderText({ c_value() })
    #'
    #'     exportTestValues(
    #'       a_squared = { a_squared() },
    #'       b_squared = { b_squared() },
    #'       c_squared = { c_squared() }
    #'     )
    #'   }
    #' )
    #'
    #' app <- AppDriver$new(shiny_app)
    #'
    #' # Show all known values
    #' str(app$get_values())
    #' #> List of 3
    #' #> $ input :List of 2
    #' #> ..$ A: int 3
    #' #> ..$ B: int 4
    #' #> $ output:List of 1
    #' #> ..$ C: chr "5"
    #' #> $ export:List of 3
    #' #> ..$ a_squared: int 9
    #' #> ..$ b_squared: int 16
    #' #> ..$ c_squared: int 25
    #'
    #' # Get only `export` values
    #' str(app$get_values(export = TRUE))
    #' #> List of 1
    #' #> $ export:List of 3
    #' #> ..$ a_squared: int 9
    #' #> ..$ b_squared: int 16
    #' #> ..$ c_squared: int 25
    #'
    #' # Get values `"A"` from `input` and `"C"` from `output`
    #' str(app$get_values(input = "A", output = "C"))
    #' #> List of 2
    #' #> $ input :List of 1
    #' #> ..$ A: int 3
    #' #> $ output:List of 1
    #' #> ..$ C: chr "5"
    #' }
    get_values = function(
      ...,
      input = missing_arg(), output = missing_arg(), export = missing_arg(),
      hash_images = FALSE
    ) {
      app_get_values(
        self, private,
        input = input, output = output, export = export,
        ...,
        hash_images = hash_images
      )
    },

    #' @description
    #' Expect a downloadable file
    #'
    #' Given a [shiny::downloadButton()]/[shiny::downloadLink()] `output` ID, the corresponding
    #' file will be downloaded and saved as a snapshot file.
    #'
    #' @param output `output` ID of [shiny::downloadButton()]/[shiny::downloadLink()]
    #' @param name File name to save file to (including file name extension). The default, `NULL`,
    #'   generates an ascending sequence of names: `001.download`,
    #'   `002.download`, etc.
    #' @param compare This value is passed through to [`testthat::expect_snapshot_file()`].
    #'   By default it is set to `NULL` which will default to `testthat::compare_file_text` if `name`
    #'   has extension `.r`, `.R`, `.Rmd`, `.md`, or `.txt`, and otherwise uses
    #'   `testthat::compare_file_binary`.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/10_download", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Save snapshot of rock.csv as 001.download
    #' # Save snapshot value of `rock.csv` to capture default file name
    #' app$expect_download("downloadData", compare = testthat::compare_file_text)
    #' }
    expect_download = function(output, ..., compare = NULL, name = NULL, cran = FALSE) {
      app_expect_download(self, private, output = output, ..., compare = compare, name = name, cran = cran)
    },
    #' @description
    #' Get downloadable file
    #'
    #' Given a [shiny::downloadButton()]/[shiny::downloadLink()] `output` ID, the corresponding
    #' file will be downloaded and saved as a file.
    #'
    #' @param output `output` ID of [shiny::downloadButton()]/[shiny::downloadLink()]
    #' @param filename File path to save the downloaded file to.
    #' @return
    #' `$get_download()` will return the final save location of the file. This
    #' location can change depending on the value of `filename` and response
    #' headers.
    #'
    #' Location logic:
    #' * If `filename` is not NULL, `filename` will be returned.
    #' * If a [\code{content-disposition} \code{filename}](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition) is provided,
    #' then a temp file containing this `filename` will be
    #' returned.
    #' * Otherwise, a temp file ending in `.download` will be returned.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/10_download", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Get rock.csv as a tempfile
    #' app$get_download("downloadData")
    #' #> [1] "/TEMP/PATH/rock.csv"
    #'
    #' # Get rock.csv as a "./myfile.csv"
    #' app$get_download("downloadData", filename = "./myfile.csv")
    #' #> [1] "./myfile.csv"
    #' }
    get_download = function(output, filename = NULL) {
      app_get_download(self, private, output = output, filename = filename)
    },

    #' @description
    #' Expect snapshot of UI text
    #'
    #' `$expect_text()` will extract the text value of all matching elements via
    #' `TAG.textContent` and store them in a snapshot file. This method is more
    #' robust to internal package change as only the text values will be
    #' maintained. Note, this method will not retrieve any `<input />` value's
    #' text content, e.g. text inputs or text areas, as the input values are not
    #' stored in the live HTML.
    #'
    #' When possible, use `$expect_text()` over `$expect_html()` to allow
    #' package authors room to alter their HTML structures. The resulting array
    #' of `TAG.textContent` values found will be stored in a snapshot file.
    #'
    #' Please see [Robust testing](https://rstudio.github.io/shinytest2/articles/robust.html) for more details.
    #'
    #' @param selector A DOM CSS selector to be passed into `document.querySelectorAll()`
    #' @examples
    #' \dontrun{
    #' hello_app <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(hello_app)
    #'
    #' # Make a snapshot of `"Hello Shiny!"`
    #' app$expect_text("h2")
    #' }
    expect_text = function(selector, ..., cran = FALSE) {
      app_expect_text(self, private, selector, ..., cran = cran)
    },
    #' @description
    #' Get UI text
    #'
    #' `$get_text()` will extract the text value of all matching elements via
    #' `TAG.textContent`. Note, this method will not retrieve any `<input />`
    #' value's text content, e.g. text inputs or text areas, as the input values
    #' are not stored in the live HTML.
    #'
    #' @param selector A DOM CSS selector to be passed into `document.querySelectorAll()`
    #' @return A vector of character values
    #' @examples
    #' \dontrun{
    #' hello_app <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(hello_app)
    #'
    #' app$get_text("h2")
    #' #> [1] "Hello Shiny!"
    #' }
    get_text = function(selector) {
      app_get_text(self, private, selector = selector)
    },


    #' @description Expect snapshot of UI HTML
    #'
    #' `$expect_html()` will extract the full DOM structures of each matching
    #' element and store them in a snapshot file. This method captures internal
    #' DOM structure which may be brittle to changes by external authors or
    #' dependencies.
    #'
    #' Note, this method will not retrieve any `<input />` value's
    #' text content, e.g. text inputs or text areas, as the input values are not
    #' stored in the live HTML.
    #'
    #' When possible, use `$expect_text()` over `$expect_html()` to allow
    #' package authors room to alter their HTML structures. The resulting array
    #' of `TAG.textContent` values found will be stored in a snapshot file.
    #'
    #' Please see [Robust testing](https://rstudio.github.io/shinytest2/articles/robust.html) for more details.
    #'
    #' @param selector A DOM selector to be passed into `document.querySelectorAll()`
    #' @param outer_html If `TRUE` (default), the full DOM structure will be returned (`TAG.outerHTML`).
    #'   If `FALSE`, the full DOM structure of the child elements will be returned (`TAG.innerHTML`).
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/04_mpg", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Save a snapshot of the `caption` output
    #' app$expect_html("#caption")
    #' }
    expect_html = function(selector, ..., outer_html = TRUE, cran = FALSE) {
      app_expect_html(self, private, selector, ..., outer_html = outer_html, cran = cran)
    },
    #' @description Get UI HTML
    #'
    #' `$get()` will extract the full DOM structures of each matching
    #' element. This method captures internal
    #' DOM structure which may be brittle to changes by external authors or
    #' dependencies.
    #'
    #' Note, this method will not retrieve any `<input />` value's
    #' text content, e.g. text inputs or text areas, as the input values are not
    #' stored in the live HTML.
    #'
    #' Please see [Robust testing](https://rstudio.github.io/shinytest2/articles/robust.html) for more details.
    #'
    #' @param selector A DOM selector to be passed into `document.querySelectorAll()`
    #' @param outer_html If `TRUE`, the full DOM structure will be returned (`TAG.outerHTML`).
    #'   If `FALSE`, the full DOM structure of the child elements will be returned (`TAG.innerHTML`).
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/03_reactivity", package = "shiny")
    #' app <- AppDriver$new(app_path, check_names = FALSE)
    #'
    #' app$set_inputs(caption = "Custom value!")
    #' cat(app$get_html(".shiny-input-container")[1])
    #' #> <div class="form-group shiny-input-container">
    #' #>   <label class="control-label" id="caption-label" for="caption">Caption:</label>
    #' #>   <input id="caption" type="text" class="form-control shiny-bound-input" value="Data Summary">
    #' #> </div>
    #' ## ^^ No update to the DOM of `caption`
    #' }
    get_html = function(selector, ..., outer_html = TRUE) {
      app_get_html(self, private, selector, ..., outer_html = outer_html)
    },

    #' @description
    #' Expect snapshot of JavaScript script output
    #'
    #' This is a building block function that may be called by other functions.
    #' For example, `$expect_text()` and `$expect_html()` are thin wrappers around this function.
    #'
    #' Once the `script` has executed, the JSON result will be saved to a snapshot file.
    #'
    #' @param script A string containing the JavaScript script to be executed.
    #' @param file A file containing JavaScript code to be read and used as the `script`. Only one of `script` or `file` can be specified.
    #' @param pre_snapshot A function to be called on the result of the script before taking the snapshot.
    #'   `$expect_html()` and `$expect_text()` both use [`unlist()`].
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/07_widgets", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Track how many clicks are given to `#update` button
    #' app$run_js("
    #'   window.test_counter = 0;
    #'   $('#update').click(() => window.test_counter++);
    #' ")
    #' app$set_inputs(obs = 20)
    #' # Click the update button, incrementing the counter
    #' app$click("update")
    #' # Save a snapshot of number of clicks (1)
    #' app$expect_js("window.test_counter;")
    #' }
    expect_js = function(
      script = missing_arg(),
      ...,
      file = missing_arg(),
      timeout = missing_arg(),
      pre_snapshot = NULL,
      cran = FALSE
    ) {
      app_expect_js(
        self, private,
        script = script,
        ...,
        file = file, timeout = timeout, pre_snapshot = pre_snapshot, cran = cran
      )
    },

    #' @description
    #' Execute JavaScript code in the browser and return the result
    #'
    #' This function will block the local R session until the code has finished
    #' executing its _tick_ in the browser. If a `Promise` is returned from the
    #' script, `$get_js()` will wait for the promise to resolve. To have
    #' JavaScript code execute asynchronously, wrap the code in a Promise object
    #' and have the script return an atomic value.
    #'
    #' Arguments will have to be inserted into the script as there is not access
    #' to `arguments`. This can be done with commands like `paste()`. If using
    #' `glue::glue()`, be sure to use uncommon `.open` and `.close` values to
    #' avoid having to double all `{` and `}`.
    #' @param script JavaScript to execute. If a JavaScript Promise is returned,
    #'   the R session will block until the promise has been resolved and return
    #'   the value.
    #' @param file A (local) file containing JavaScript code to be read and used
    #'   as the `script`. Only one of `script` or `file` can be specified.
    #' @return Result of the `script` (or `file` contents)
    #' @examples
    #' \dontrun{
    #' library(shiny)
    #' shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
    #' app <- AppDriver$new(shiny_app)
    #'
    #' # Execute JavaScript code in the app's browser
    #' app$get_js("1 + 1;")
    #' #> [1] 2
    #'
    #' # Execute a JavaScript Promise. Return the resolved value.
    #' app$get_js("
    #'   new Promise((resolve) => {
    #'     setTimeout(() => resolve(1 + 1), 1000)
    #'   }).
    #'   then((value) => value + 1);
    #' ")
    #' #> [1] 3
    #'
    #' # With escaped arguments
    #' loc_field <- "hostname"
    #' js_txt <- paste0("window.location[", jsonlite::toJSON(loc_field, auto_unbox = TRUE), "]")
    #' app$get_js(js_txt)
    #' #> [1] "127.0.0.1"
    #'
    #' # With `glue::glue()`
    #' js_txt <- glue::glue_data(
    #'   lapply(
    #'     list(x = 40, y = 2),
    #'     jsonlite::toJSON,
    #'     auto_unbox = TRUE
    #'   ),
    #'   .open = "<", .close = ">",
    #'   "let answer = function(a, b) {\n",
    #'   "  return a + b;\n",
    #'   "};\n",
    #'   "answer(<x>, <y>);\n"
    #' )
    #' app$get_js(js_txt)
    #' #> [1] 42
    #' }
    get_js = function(
      script = missing_arg(),
      ...,
      file = missing_arg(),
      timeout = missing_arg()
    ) {
      app_get_js(
        self, private,
        script = script,
        ...,
        file = file,
        timeout = timeout
      )
    },
    #' @description
    #' Execute JavaScript code in the browser
    #'
    #' This function will block the local R session until the code has finished
    #' executing its _tick_ in the browser.
    #'
    #' The final result of the code will be ignored and not returned to the R session.
    #' @param script JavaScript to execute.
    #' @param file A (local) file containing JavaScript code to be read and used
    #'   as the `script`. Only one of `script` or `file` can be specified.
    #' @examples
    #' \dontrun{
    #' library(shiny)
    #' shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
    #' app <- AppDriver$new(shiny_app)
    #'
    #' # Get JavaScript answer from the app's browser
    #' app$get_js("1 + 1")
    #' #> [1] 2
    #' # Execute JavaScript code in the app's browser
    #' app$run_js("1 + 1")
    #' # (Returns `app` invisibly)
    #'
    #' # With escaped arguments
    #' loc_field <- "hostname"
    #' js_txt <- paste0("window.location[", jsonlite::toJSON(loc_field, auto_unbox = TRUE), "]")
    #' app$run_js(js_txt)
    #' app$get_js(js_txt)
    #' #> [1] "127.0.0.1"
    #' }
    run_js = function(
      script = missing_arg(),
      ...,
      file = missing_arg(),
      timeout = missing_arg()
    ) {
      app_run_js(
        self, private,
        script = script,
        ...,
        file = file,
        timeout = timeout
      )
    },


    #' @description
    #' Expect a screenshot of the Shiny application
    #'
    #' This method takes a screenshot of the application (of only the `selector`
    #' area) and compares the image to the expected image.
    #'
    #' Please be aware that this method is very brittle to changes outside of your Shiny application.
    #' These changes can include:
    #' * running on a different R version
    #' * running on a different in operating system
    #' * using a different default system font
    #' * using different package versions
    #' These differences are explicitly clear when working with plots.
    #'
    #' Unless absolutely necessary for application consistency, it is strongly
    #' recommended to use other expectation methods.
    #'
    #' @param screenshot_args This named list of arguments is passed along to
    #'   [`chromote::ChromoteSession`]'s `$get_screenshot()` method. If missing, the
    #'   value will default to `$new(screenshot_args=)`.
    #'
    #' If the value is:
    #'   * `TRUE`: A screenshot of the whole page will be taken with no delay
    #'   * A named list of arguments: Arguments passed directly to
    #'     [`chromote::ChromoteSession`]'s `$get_screenshot()` method. The `selector`
    #'     and `delay` will default to `"html"` and `0` respectively.
    #'
    #' If `FALSE` is provided, the parameter will be ignored and a
    #' screenshot will be taken with default behavior.
    #' @param name The file name to be used for the snapshot. The file extension
    #'   will overwritten to `.png`. By default, the `name` supplied to
    #'   `app` on initialization with a counter will be used (e.g. `"NAME-001.png"`).
    #' @param compare A function used to compare the screenshot snapshot files.
    #' The function should take two inputs, the paths to the `old` and `new`
    #' snapshot, and return either `TRUE` or `FALSE`.
    #'
    #' `compare` defaults to a function that wraps around
    #' `compare_screenshot_threshold(old, new, threshold = threshold,
    #' kernel_size = kernel_size, quiet = quiet)`. Note: if `threshold` is
    #' `NULL` (default), `compare` will behave as if
    #' [`testthat::compare_file_binary()`] was provided, comparing the two
    #' images byte-by-byte.
    #' @param threshold Parameter supplied to [`compare_screenshot_threshold()`]
    #' when using the default `compare` method. If the value of `threshold` is
    #' NULL`, [`compare_screenshot_threshold()`] will act like
    #' [`testthat::compare_file_binary`]. However, if `threshold` is a positive number,
    #' it will be compared against the largest convolution value found if the
    #' two images fail a [`testthat::compare_file_binary`] comparison.
    #'
    #' Which value should I use? Threshold values values below 5 help deter
    #' false-positive screenshot comparisons (such as inconsistent rounded
    #' corners). Larger values in the 10s and 100s will help find _real_
    #' changes. However, not all values are one size fits all and you will need
    #' to play with a threshold that fits your needs.
    #' @param kernel_size Parameter supplied to [`compare_screenshot_threshold()`]
    #' when using the default `compare` method. The `kernel_size` represents the
    #' height and width of the convolution kernel applied to the pixel
    #' differences. This integer-like value should be relatively small.
    #' @param quiet Parameter supplied to [`compare_screenshot_threshold()`]
    #' when using the default `compare` method. If `FALSE`, diagnostic
    #' information will be presented when the computed value is larger than a
    #' non-`NULL` `threshold` value.
    #' @examples
    #' \dontrun{
    #' # These example lines should be performed in a `./tests/testthat`
    #' # test file so that snapshot files can be properly saved
    #'
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path, variant = platform_variant())
    #'
    #' # Expect a full size screenshot to be pixel perfect
    #' app$expect_screenshot()
    #'
    #' # Images are brittle when containing plots
    #' app$expect_screenshot(selector = "#distPlot")
    #'
    #' # Test with more threshold in pixel value differences
    #' # Helps with rounded corners
    #' app$expect_screenshot(threshold = 10)
    #'
    #' # Equivalent expectations
    #' app$expect_screenshot() # default
    #' app$expect_screenshot(threshold = NULL)
    #' app$expect_screenshot(compare = testthat::compare_file_binary)
    #' expect_snapshot_file(
    #'   app$get_screenshot(),
    #'   variant = app$get_variant(),
    #'   compare = testthat::compare_file_binary
    #' )
    #'
    #' # Equivalent expectations
    #' app$expect_screenshot(threshold = 3, kernel_size = 5)
    #' app$expect_screenshot(compare = function(old, new) {
    #'   compare_screenshot_threshold(
    #'     old, new,
    #'     threshold = 3,
    #'     kernel_size = 5
    #'   )
    #' })
    #' expect_screenshot_file(
    #'   app$get_screenshot(),
    #'   variant = app$get_variant(),
    #'   compare = function(old, new) {
    #'     compare_screenshot_threshold(
    #'       old, new,
    #'       threshold = 3,
    #'       kernel_size = 5
    #'     )
    #'   }
    #' )
    #' }
    expect_screenshot = function(
      ...,
      threshold = NULL,
      kernel_size = 5,
      screenshot_args = missing_arg(),
      delay = missing_arg(),
      selector = missing_arg(),
      compare = missing_arg(),
      quiet = FALSE,
      name = NULL,
      cran = FALSE
    ) {
      app_expect_screenshot_and_variant(
        self, private,
        ...,
        threshold = threshold,
        kernel_size = kernel_size,
        quiet = quiet,
        compare = compare,
        screenshot_args = screenshot_args,
        delay = delay,
        selector = selector,
        name = name,
        cran = cran
      )
    },
    #' @description
    #' Take a screenshot
    #'
    #' Take a screenshot of the Shiny application.
    #'
    #' @param file If `NULL`, then the image will be displayed to the current
    #'   Graphics Device. If a file path, then the screenshot will be saved to
    #'   that file.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Display in graphics device
    #' app$get_screenshot()
    #'
    #' # Update bins then display `"disPlot"` in graphics device
    #' app$set_inputs(bins = 10)
    #' app$get_screenshot(selector = "#distPlot")
    #'
    #' # Save screenshot to file and view it
    #' tmpfile <- tempfile(fileext = ".png")
    #' app$get_screenshot(tmpfile)
    #' showimage::show_image(tmpfile)
    #' }
    get_screenshot = function(
      file = NULL,
      ...,
      screenshot_args = missing_arg(),
      delay = missing_arg(),
      selector = missing_arg()
    ) {
      app_get_screenshot(
        self, private,
        file = file,
        ...,
        screenshot_args = screenshot_args,
        delay = delay,
        selector = selector
      )
    },


    #' @description Wait for Shiny to not be busy (idle) for a set amount of time
    #'
    #' Waits until Shiny has not been busy for a set duration of time, e.g. no
    #' reactivity is updating or has occurred.
    #'
    #' This is useful, for example, when waiting for your application to
    #' initialize or if you've resized the window with `$set_window_size()` and
    #' want to make sure all plot redrawing is complete before take a
    #' screenshot.
    #'
    #' By default,
    #' * `$new(wait = TRUE)` waits for Shiny to not be busy after initializing
    #' the application
    #' * `$set_window_size(wait = TRUE)` waits for Shiny to not be busy after
    #' resizing the window.)
    #' @param duration How long Shiny must be idle (in ms) before unblocking the
    #'   R session.
    #' @return `invisible(self)` if Shiny stabilizes within the `timeout`.
    #'   Otherwise an error will be thrown
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' pre_value <- app$get_value(output = "distPlot")
    #' # Update bins value
    #' app$set_inputs(bins = 10, wait_ = FALSE)
    #' middle_value <- app$get_value(output = "distPlot")
    #' app$wait_for_idle()
    #' post_value <- app$get_value(output = "distPlot")
    #'
    #' # No guarantee that these values are different
    #' identical(pre_value, middle_value)
    #' # Will not be equal
    #' identical(pre_value, post_value)
    #'
    #' # ---------------------
    #' ## Change the screen size to trigger a plot update
    #' pre_value <- app$get_value(output = "distPlot")
    #' app$set_window_size(height = 1080, width = 1920, wait = FALSE)
    #' middle_value <- app$get_value(output = "distPlot")
    #' app$wait_for_idle()
    #' post_value <- app$get_value(output = "distPlot")
    #'
    #' # No guarantee that these values are different
    #' identical(pre_value, middle_value)
    #' # Will not be equal
    #' identical(pre_value, post_value)
    #' }
    wait_for_idle = function(duration = 500, timeout = missing_arg()) {
      app_wait_for_idle(self, private, duration = duration, timeout = timeout)
    },

    #' @description Wait for a new Shiny value
    #'
    #' Waits until an `input`, `output`, or `export` Shiny value is not one of
    #' `ignore`d values, or the `timeout` is reached.
    #'
    #' Only a single `input`, `output`, or `export` value may be used.
    #'
    #' This function can be useful in helping determine if an application
    #' has finished processing a complex reactive situation.
    #' @param input,output,export A name of an input, output, or export value.
    #'   Only one of these parameters may be used.
    #' @param ignore List of possible values to ignore when checking for
    #'   updates.
    #' @param timeout_ Amount of time to wait for a new `output` value before giving up (milliseconds).
    #'   Defaults to the resolved `timeout` value during the `AppDriver` initialization.
    #' @param interval How often to check for the condition, in ms.
    #' @return Newly found value
    #' @examples
    #' \dontrun{
    #' library(shiny)
    #' shiny_app <- shinyApp(
    #'   fluidPage(
    #'     h1("Dynamic output"),
    #'     actionButton("display", "Display UI"),
    #'     uiOutput("dym1"),
    #'   ),
    #'   function(input, output) {
    #'     output$dym1 <- renderUI({
    #'       req(input$display)
    #'       Sys.sleep(runif(1, max = 2)) # Artificial calculations
    #'       tagList(
    #'         sliderInput("slider1", "Slider #1", 0, 100, 25),
    #'         uiOutput("dym2")
    #'       )
    #'     })
    #'     output$dym2 <- renderUI({
    #'       Sys.sleep(runif(1, max = 2)) # Artificial calculations
    #'       tagList(
    #'         sliderInput("slider2", "Slider #2", 0, 100, 50),
    #'         "Total:", verbatimTextOutput("total")
    #'       )
    #'     })
    #'     output$total <- renderText({
    #'       req(input$slider1, input$slider2)
    #'       input$slider1 + input$slider2
    #'     })
    #'   }
    #' )
    #'
    #' app <- AppDriver$new(shiny_app)
    #'
    #' # Create UI / output values
    #' app$click("display")
    #' # Wait for total to be calculated (or have a non-NULL value)
    #' new_total_value <- app$wait_for_value(output = "total")
    #' #> [1] "75"
    #' app$get_value(output = "total")
    #' #> [1] "75"
    #' }
    wait_for_value = function(
      ...,
      input = missing_arg(),
      output = missing_arg(),
      export = missing_arg(),
      ignore = list(NULL, ""),
      timeout = missing_arg(),
      interval = 400
    ) {
      app_wait_for_value(
        self, private,
        input = input, output = output, export = export,
        ...,
        ignore = ignore,
        timeout = timeout, interval = interval
      )
    },
    #' @description Wait for a JavaScript expression to be true
    #'
    #' Waits until a JavaScript `expr`ession evaluates to `true` or the
    #' `timeout` is exceeded.
    #'
    #' @param script A string containing JavaScript code. This code must
    #'   eventually return a [`true`thy
    #'   value](https://developer.mozilla.org/en-US/docs/Glossary/Truthy) or a
    #'   timeout error will be thrown.
    #' @param timeout How long the script has to return a `true`thy value (milliseconds).
    #'   Defaults to the resolved `timeout` value during the `AppDriver` initialization.
    #' @param interval How often to check for the condition (milliseconds).
    #' @return `invisible(self)` if expression evaluates to `true` without error
    #'   within the timeout. Otherwise an error will be thrown
    #' @examples
    #' \dontrun{
    #' shiny_app <- shinyApp(h1("Empty App"), function(input, output) { })
    #' app <- AppDriver$new(shiny_app)
    #'
    #' # Contrived example:
    #' # Wait until `Date.now()` returns a number that ends in a 5. (0 - 10 seconds)
    #' system.time(
    #'   app$wait_for_js("Math.floor((Date.now() / 1000) % 10) == 5;")
    #' )
    #'
    #' ## A second example where we run the contents of a JavaScript file
    #' ## and use the result to wait for a condition
    #' app$run_js(file = "complicated_file.js")
    #' app$wait_for_js("complicated_condition();")
    #' }
    wait_for_js = function(
      script,
      timeout = missing_arg(),
      interval = 100
    ) {
      app_wait_for_js(
        self, private,
        script = script,
        timeout = timeout,
        interval = interval
      )
    },



    #' @description
    #' Expect unique input and output names.
    #'
    #' If the HTML has duplicate input or output elements with matching `id`
    #' values, this function will throw an error. It is similar to
    #' `AppDriver$new(check_names = TRUE)`, but asserts that no warnings are
    #' displayed.
    #'
    #' This method will not throw if a single input and a single output have the
    #' same name.
    #' @examples
    #' \dontrun{
    #' shiny_app <- shinyApp(
    #'   ui = fluidPage(
    #'     # Duplicate input IDs: `"text"`
    #'     textInput("text", "Text 1"),
    #'     textInput("text", "Text 2")
    #'   ),
    #'   server = function(input, output) {
    #'     # empty
    #'   }
    #' )
    #' # Initial checking for unique names (default behavior)
    #' app <- AppDriver$new(shiny_app, check_names = TRUE)
    #' #> Warning:
    #' #> ! Shiny inputs should have unique HTML id values.
    #' #> i The following HTML id values are not unique:
    #' #> • text
    #' app$stop()
    #'
    #' # Manually assert that all names are unique
    #' app <- AppDriver$new(shiny_app, check_names = FALSE)
    #' app$expect_unique_names()
    #' #> Error: `app_check_unique_names(self, private)` threw an unexpected warning.
    #' #> Message: ! Shiny inputs should have unique HTML id values.
    #' #> i The following HTML id values are not unique:
    #' #>   • text
    #' #> Class:   rlang_warning/warning/condition
    #' app$stop()
    #' }
    expect_unique_names = function() {
      app_expect_unique_names(self, private)
    },


    #' @description
    #' Retrieve the Shiny app path
    #'
    #' @return The directory containing the Shiny application or Shiny runtime
    #'   document. If a URL was provided to `app_dir` during initialization, the
    #'   current directory will be returned.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' identical(app$get_dir(), app_path)
    #' #> [1] TRUE
    #' }
    get_dir = function() {
      app_get_dir(self, private)
    },
    #' @description
    #' Retrieve the Shiny app URL
    #'
    #' @return URL where the Shiny app is being hosted
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' browseURL(app$get_url())
    #' }
    get_url = function() {
      private$shiny_url$get()
    },

    #' @description
    #' Get window size
    #'
    #' Get current size of the browser window, as list of numeric scalars
    #'   named `width` and `height`.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' app$get_window_size()
    #' #> $width
    #' #> [1] 992
    #' #>
    #' #> $height
    #' #> [1] 1323
    #' }
    get_window_size = function() {
      app_get_window_size(self, private)
    },
    #' @description
    #' Sets size of the browser window.
    #' @param width,height Height and width of browser, in pixels.
    #' @param wait If `TRUE`, `$wait_for_idle()` will be called after setting
    #'   the window size. This will block until any width specific items (such
    #'   as plots) that need to be re-rendered.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' # Set init window size
    #' app <- AppDriver$new(app_path, height = 1400, width = 1000)
    #'
    #' app$get_window_size()
    #' #> $width
    #' #> [1] 1000
    #' #>
    #' #> $height
    #' #> [1] 1400
    #'
    #' # Manually set the window size
    #' app$set_window_size(height = 1080, width = 1920)
    #' app$get_window_size()
    #' #> $width
    #' #> [1] 1920
    #' #>
    #' #> $height
    #' #> [1] 1080
    #' }
    set_window_size = function(width, height, wait = TRUE) {
      app_set_window_size(self, private, width = width, height = height, wait = wait)
    },

    #' @description
    #' Get Chromote Session
    #'
    #' Get the [`ChromoteSession`] object from the \pkg{chromote} package.
    #' @return [`ChromoteSession`] R6 object
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' b <- app$get_chromote_session()
    #' b$Runtime$evaluate("1 + 1")
    #' #> $result
    #' #> $result$type
    #' #> [1] "number"
    #' #>
    #' #> $result$value
    #' #> [1] 2
    #' #>
    #' #> $result$description
    #' #> [1] "2"
    #' }
    get_chromote_session = function() {
      app_get_chromote_session(self, private)
    },
    #' @description
    #' Get the variant
    #'
    #' Get the `variant` supplied during initialization
    #' @return The `variant` value supplied during initialization or `NULL` if
    #'   no value was supplied.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #'
    #' app <- AppDriver$new(app_path)
    #'
    #' app$get_variant()
    #' #> NULL
    #'
    #' app <- AppDriver$new(app_path, variant = platform_variant())
    #' app$get_variant()
    #' #> [1] "mac-4.1"
    #' }
    get_variant = function() {
      app_get_variant(self, private)
    },

    #' @description
    #' Get all logs
    #'
    #' Retrieve all of the debug logs that have been recorded.
    #' @return A data.frame with the following columns:
    #' * `workerid`: The shiny worker ID found within the browser
    #' * `timestamp`: POSIXct timestamp of the message
    #' * `location`: The location of the message was found. One of three values:
    #'   * `"shinytest2"`: Occurs when `$log_message()` is called
    #'   * `"shiny"`: `stdin` and `stdout` messages from the Shiny server. Note `message()` output is sent to `stdout`.
    #'   * `"chromote"`: Captured by the \pkg{chromote} event handlers. See
    #'      [console API](https://chromedevtools.github.io/devtools-protocol/1-3/Runtime/#event-consoleAPICalled),
    #'      [exception thrown](https://chromedevtools.github.io/devtools-protocol/1-3/Runtime/#event-exceptionThrown),
    #'      [websocket sent](https://chromedevtools.github.io/devtools-protocol/1-3/Network/#event-webSocketFrameSent), and
    #'      [websocket received](https://chromedevtools.github.io/devtools-protocol/1-3/Network/#event-webSocketFrameReceived)
    #'      for more details
    #' * `level`: For a given location, there are different types of log levels.
    #'   * `"shinytest2"`: `"log"`; Only log messages are captured.
    #'   * `"shiny"`: `"stdout"` or `"stderr"`; Note, `message()` output is sent
    #'     to `stderr`.
    #'   * `"chromote"`: Correspond to any level of a JavaScript
    #'     `console.LEVEL()` function call. Typically, these are "log"` and
    #'     `"error"` but can include `"info"`, `"debug"`, and `"warn"`. If
    #'     `options(shiny.trace = TRUE)`, then the level will recorded as
    #'     `"websocket"`.
    #' @examples
    #' \dontrun{
    #' app1 <- AppDriver$new(system.file("examples/01_hello", package = "shiny"))
    #'
    #' app1$get_logs()
    #' #> \{shinytest2\} R  info   10:00:28.86 Start AppDriver initialization
    #' #> \{shinytest2\} R  info   10:00:28.86 Starting Shiny app
    #' #> \{shinytest2\} R  info   10:00:29.76 Creating new ChromoteSession
    #' #> \{shinytest2\} R  info   10:00:30.56 Navigating to Shiny app
    #' #> \{shinytest2\} R  info   10:00:30.70 Injecting shiny-tracer.js
    #' #> \{chromote\}   JS info   10:00:30.75 shinytest2; jQuery found
    #' #> \{chromote\}   JS info   10:00:30.77 shinytest2; Waiting for shiny session to connect
    #' #> \{chromote\}   JS info   10:00:30.77 shinytest2; Loaded
    #' #> \{shinytest2\} R  info   10:00:30.77 Waiting for Shiny to become ready
    #' #> \{chromote\}   JS info   10:00:30.90 shinytest2; Connected
    #' #> \{chromote\}   JS info   10:00:30.95 shinytest2; shiny:busy
    #' #> \{shinytest2\} R  info   10:00:30.98 Waiting for Shiny to become idle for 200ms within 15000ms
    #' #> \{chromote\}   JS info   10:00:30.98 shinytest2; Waiting for Shiny to be stable
    #' #> \{chromote\}   JS info   10:00:31.37 shinytest2; shiny:idle
    #' #> \{chromote\}   JS info   10:00:31.38 shinytest2; shiny:value distPlot
    #' #> \{chromote\}   JS info   10:00:31.57 shinytest2; Shiny has been idle for 200ms
    #' #> \{shinytest2\} R  info   10:00:31.57 Shiny app started
    #' #> \{shiny\}      R  stderr ----------- Loading required package: shiny
    #' #> \{shiny\}      R  stderr ----------- Running application in test mode.
    #' #> \{shiny\}      R  stderr -----------
    #' #> \{shiny\}      R  stderr ----------- Listening on http://127.0.0.1:4679
    #'
    #'
    #' # To capture all websocket traffic, set `options = list(shiny.trace = TRUE)`
    #' app2 <- AppDriver$new(
    #'   system.file("examples/01_hello", package = "shiny"),
    #'   options = list(shiny.trace = TRUE)
    #' )
    #'
    #' app2$get_logs()
    #' ## (All WebSocket messages have been replaced with `WEBSOCKET_MSG` in example below)
    #' #> \{shinytest2\} R  info      10:01:57.49 Start AppDriver initialization
    #' #> \{shinytest2\} R  info      10:01:57.50 Starting Shiny app
    #' #> \{shinytest2\} R  info      10:01:58.20 Creating new ChromoteSession
    #' #> \{shinytest2\} R  info      10:01:58.35 Navigating to Shiny app
    #' #> \{shinytest2\} R  info      10:01:58.47 Injecting shiny-tracer.js
    #' #> \{chromote\}   JS info      10:01:58.49 shinytest2; jQuery not found
    #' #> \{chromote\}   JS info      10:01:58.49 shinytest2; Loaded
    #' #> \{shinytest2\} R  info      10:01:58.50 Waiting for Shiny to become ready
    #' #> \{chromote\}   JS info      10:01:58.55 shinytest2; jQuery found
    #' #> \{chromote\}   JS info      10:01:58.55 shinytest2; Waiting for shiny session to connect
    #' #> \{chromote\}   JS websocket 10:01:58.64 send WEBSOCKET_MSG
    #' #> \{chromote\}   JS websocket 10:01:58.67 recv WEBSOCKET_MSG
    #' #> \{chromote\}   JS info      10:01:58.67 shinytest2; Connected
    #' #> \{chromote\}   JS websocket 10:01:58.71 recv WEBSOCKET_MSG
    #' #> \{chromote\}   JS websocket 10:01:58.72 recv WEBSOCKET_MSG
    #' #> \{chromote\}   JS info      10:01:58.72 shinytest2; shiny:busy
    #' #> \{chromote\}   JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #' #> \{chromote\}   JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #' #> \{shinytest2\} R  info      10:01:58.75 Waiting for Shiny to become idle for 200ms within 15000ms
    #' #> \{chromote\}   JS info      10:01:58.75 shinytest2; Waiting for Shiny to be stable
    #' #> \{chromote\}   JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #' #> \{chromote\}   JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #' #> \{chromote\}   JS info      10:01:58.81 shinytest2; shiny:idle
    #' #> \{chromote\}   JS websocket 10:01:58.82 recv WEBSOCKET_MSG
    #' #> \{chromote\}   JS info      10:01:58.82 shinytest2; shiny:value distPlot
    #' #> \{chromote\}   JS info      10:01:59.01 shinytest2; Shiny has been idle for 200ms
    #' #> \{shinytest2\} R  info      10:01:59.01 Shiny app started
    #' #> \{shiny\}      R  stderr    ----------- Loading required package: shiny
    #' #> \{shiny\}      R  stderr    ----------- Running application in test mode.
    #' #> \{shiny\}      R  stderr    -----------
    #' #> \{shiny\}      R  stderr    ----------- Listening on http://127.0.0.1:4560
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"config":\{"workerId":"","sessionId"|truncated
    #' #> \{shiny\}      R  stderr    ----------- RECV \{"method":"init","data":\{"bins":30,|truncated
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"busy":"busy"\}
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"recalculating":\{"name":"distPlot",|truncated
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"recalculating":\{"name":"distPlot",|truncated
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"busy":"idle"\}
    #' #> \{shiny\}      R  stderr    ----------- SEND \{"errors":\{\},"values":\{"distPlot":|truncated
    #'
    #' # The log that is returned is a `data.frame()`.
    #' log <- app2$get_logs()
    #' tibble::glimpse(log)
    #' #> Rows: 43
    #' #> Columns: 5
    #' #> $ workerid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    #' #> $ timestamp <dttm> 2022-09-19 10:01:57, 2022-09-19 10:01:57, 2022-09-19 10:01:58, 2022…
    #' #> $ location  <chr> "shinytest2", "shinytest2", "shinytest2", "shinytest2", "shinytest2"…
    #' #> $ level     <chr> "info", "info", "info", "info", "info", "info", "info", "info", "inf…
    #' #> $ message   <chr> "Start AppDriver initialization", "Starting Shiny app", "Creating ne…
    #' #> $ workerid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    #' #> $ timestamp <dttm> 2022-03-16 11:09:57, 2022-03-16 11:09:57, 2022-03-16 11:09:…
    #' #> $ location  <chr> "shinytest2", "shinytest2", "shinytest2", "shinytest2", "shi…
    #' #> $ level     <chr> "info", "info", "info", "info", "info", "info", "info", "inf…
    #' #> $ message   <chr> "Start AppDriver initialization", "Starting Shiny app", "Cre…
    #'
    #' # It may be filtered to find desired logs
    #' subset(log, level == "websocket")
    #' ## (All WebSocket messages have been replaced with `WEBSOCKET_MSG` in example below)
    #' #> \{chromote\} JS websocket 10:01:58.64 send WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.67 recv WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.71 recv WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.72 recv WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.73 recv WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.81 recv WEBSOCKET_MSG
    #' #> \{chromote\} JS websocket 10:01:58.82 recv WEBSOCKET_MSG
    #' }
    get_logs = function() {
      app_get_logs(self, private)
    },

    #' @description
    #' Add a message to the \pkg{shinytest2} log.
    #' @param message Single message to store in log
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' app$log_message("Setting bins to smaller value")
    #' app$set_inputs(bins = 10)
    #' app$get_logs()
    #' }
    log_message = function(message) {
      app_log_message(self, private, message = message)
    },


    #' @description Stop the Shiny application driver
    #'
    #' This method stops all known processes:
    #' * The Shiny application in the background R process,
    #' * the background R process hosting the Shiny application, and
    #' * the Chromote Session instance.
    #'
    #' To stop your shiny application and return a value from `$stop()`, see
    #' [`shiny::stopApp()`]. This is useful in testing to return context
    #' information.
    #'
    #' Typically, this can be paired with a button that when clicked will call
    #' `shiny::stopApp(info)` to return `info` from the test app back to the
    #' main R session.
    #' @param signal_timeout Milliseconds to wait between sending a `SIGINT`,
    #'   `SIGTERM`, and `SIGKILL` to the Shiny process. Defaults to 500ms and does
    #'   not utilize the resolved value from `AppDriver$new(timeout=)`. However,
    #'   if \pkg{covr} is currently executing, then the `timeout` is set to
    #'   20,000ms to allow for the coverage report to be generated.
    #' @return The result of the background process if the Shiny application has
    #'   already been terminated.
    #' @examples
    #' \dontrun{
    #' rlang::check_installed("reactlog")
    #'
    #' library(shiny)
    #' shiny_app <- shinyApp(
    #'   ui = fluidPage(
    #'     actionButton("button", "Stop app and return Reactlog"),
    #'     "Click count:", textOutput("count")
    #'   ),
    #'   server = function(input, output) {
    #'     output$count <- renderText({ input$button })
    #'     observe({
    #'       req(input$button)
    #'       stopApp(shiny::reactlog())
    #'     })
    #'   }
    #' )
    #'
    #' app <- AppDriver$new(
    #'   shiny_app,
    #'   # Enable reactlog in background R session
    #'   options = list(shiny.reactlog = TRUE)
    #' )
    #'
    #' app$click("button")
    #' rlog <- app$stop()
    #' str(head(rlog, 2))
    #' #> List of 2
    #' #> $ :List of 7
    #' #> ..$ action : chr "define"
    #' #> ..$ reactId: chr "r3"
    #' #> ..$ label  : chr "Theme Counter"
    #' #> ..$ type   : chr "reactiveVal"
    #' #> ..$ value  : chr " num 0"
    #' #> ..$ session: chr "bdc7417f2fc8c84fc05c9518e36fdc44"
    #' #> ..$ time   : num 1.65e+09
    #' #> $ :List of 7
    #' #> ..$ action : chr "define"
    #' #> ..$ reactId: chr "r4"
    #' #> ..$ label  : chr "output$count"
    #' #> .. ..- attr(*, "srcref")= int [1:6] 7 32 7 45 32 45
    #' #> .. ..- attr(*, "srcfile")= chr ""
    #' #> ..$ type   : chr "observer"
    #' #> ..$ value  : chr " NULL"
    #' #> ..$ session: chr "bdc7417f2fc8c84fc05c9518e36fdc44"
    #' #> ..$ time   : num 1.65e+09
    #' }
    stop = function(signal_timeout = missing_arg()) {
      app_stop(self, private, signal_timeout = signal_timeout)
    }
  )
)
