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
#' @section Start-up failure:
#'
#' If the app throws an error during initialization, the `AppDriver` will
#' will be stored in `rlang::last_error()$app`. This allows for the "failure
#' to initialize" to be signaled while also allowing for the `app` to be
#' retrieved after any initialization error has been thrown.
#'
#' @section Exporting reactive values:
#'
#' Reactive values from within your Shiny application can be exported using the
#' method:
#' [`shiny::exportTestValues()`](https://shiny.rstudio.com/reference/shiny/latest/exportTestValues.html).
#' This under utilized method exposes internal values of your app
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
#'
#' @param ... Must be empty. Allows for parameter expansion.
#' @param arguments A list of unnamed arguments to send to the script.
#' @param timeout Amount of time to wait before giving up (milliseconds).
#' @param cran Should these expectations be verified on CRAN? By default,
#'        they are not because snapshot tests tend to be fragile
#'        because they often rely on minor details of dependencies.
#' @param wait_ Wait until all reactive updates have completed?
#' @param timeout_ Amount of time to wait before giving up (milliseconds).
#' @param hash_images If `TRUE`, images will be hashed before being returned.
#'   Otherwise, all images will return their full data64 encoded value.
#' @param screenshot_args This named list of arguments is passed along to
#'   [`chromote::ChromoteSession`]'s `$screenshot()` method. If missing, the
#'   value will default to `$new(screenshot_args=)`.
#'
#' If the value is:
#'   * `TRUE`: A screenshot of the whole page will be taken with no delay
#'   * A named list of arguments: Arguments passed directly to [`chromote::ChromoteSession`]'s
#' `$screenshot()` method. The `selector` and `delay` will default to `"html"` and `0` respectively.
#'
#' If a `FALSE` value is provided, the parameter will be ignored and a
#' screenshot will be taken with default behavior.
#' @param delay The number of milliseconds to wait before taking the screenshot.
#'   This value can either be supplied as `delay` or `screenshot_args`'s delay
#'   slot. The `delay` parameter will have preference.
#' @param selector The selector is a CSS selector that will be used to select a
#'   portion of the page to be captured. This value can either be supplied as
#'   `selector` or `screenshot_args`'s selector slot. The `selector` parameter
#'   will have preference.
#' @importFrom R6 R6Class
#' @seealso [`platform_variant()`], [`use_shinytest2()`]
#' @export
AppDriver <- R6Class(# nolint
  "AppDriver",
  cloneable = FALSE,
  private = list(
    chromote_session = "<chromote::ChromoteSession>",
    shiny_process = NULL, # `callr::r_bg()` object
    shiny_proc_value = NULL, # Output of `private$shiny_process$value()`

    counter = "<Count>",
    shiny_url = "<Url>",

    log = list(), # List of all log messages added via `$log_message()`

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
    #' @param app_dir Directory containing your Shiny application or a run-time
    #'   Shiny R Markdown document. By default, it retrieves
    #'   `test_path("../../")` to allow for interactive and testing usage.
    #' @param name Prefix value to use when saving testthat snapshot files. Ex:
    #'   `NAME-001.json`. Name **must** be unique when saving multiple snapshots
    #'   from within the same testing file. Otherwise, two different `AppDriver`
    #'   objects will be referencing the same files.
    #' @template variant
    #' @param seed An optional random seed to use before starting the application.
    #'   For apps that use R's random number generator, this can make their
    #'   behavior repeatable.
    #' @param load_timeout How long to wait for the app to load, in ms.
    #'   This includes the time to start R. Defaults to 10s when running
    #'   locally and 20s when running on CI.
    #' @param wait If `TRUE`, `$wait_for_idle(duration = 200, timeout = load_timeout)`
    #'   will be called once the app has connected a new session, blocking until the
    #'   Shiny app is idle for 200ms.
    #' @param screenshot_args Default set of arguments to pass in to
    #'   [`chromote::ChromoteSession`]'s `$screenshot()` method when taking
    #'   screnshots within `$expect_screenshot()`. To disable screenshots by
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
      load_timeout = NULL,
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
    #' @examples
    #' \dontrun{
    #' # Open app in Chrome
    #' app$view()
    #' }
    view = function() {
      app_view(self, private)
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
    #' dependencies. Note, this method will not retrieve any `<input />` value's
    #' text content, e.g. text inputs or text areas, as the input values are not
    #' stored in the live HTML.
    #'
    #' When possible, use `$expect_text()` over `$expect_html()` to allow
    #' package authors room to alter their HTML structures. The resulting array
    #' of `TAG.textContent` values found will be stored in a snapshot file.
    #'
    #' @param selector A DOM selector to be passed into `document.querySelectorAll()`
    #' @param outer_html If `TRUE` (default), the full DOM structure will be returned (`TAG.outerHTML`).
    #'   If `FALSE`, the full DOM structure of the child elements will be returned (`TAG.innerHTML`).
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/04_mpg", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #' # Save a snapshot of the `caption` output
    #' app$expect_html("#caption")
    #' }
    expect_html = function(selector, ..., outer_html = TRUE, cran = FALSE) {
      app_expect_html(self, private, selector, ..., outer_html = outer_html, cran = cran)
    },
    #' @description Get UI HTML
    #' @param selector A DOM selector to be passed into `document.querySelectorAll()`
    #' @param outer_html If `TRUE`, the full DOM structure will be returned (`TAG.outerHTML`).
    #'   If `FALSE`, the full DOM structure of the child elements will be returned (`TAG.innerHTML`).
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/03_reactivity", package = "shiny")
    #' app <- AppDriver$new(app_path, check_names = FALSE)
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
    #' This is a building block function that should be called by other functions.
    #' For example, `$expect_text()` and `$expect_html()` are thin wrappers around this function.
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
    #' app$execute_js("
    #'   window.test_counter = 0;
    #'   $('#update').click(() => window.test_counter++);
    #' ")
    #' app$set_inputs(obs = 20)
    #' # Click the update button, incrementing the counter
    #' app$click("update")
    #' # Save a snapshot of number of clicks (1)
    #' app$expect_js("return window.test_counter;")
    #' }
    expect_js = function(
      script = missing_arg(),
      ...,
      arguments = list(),
      file = missing_arg(),
      timeout = 15 * 1000,
      pre_snapshot = NULL,
      cran = FALSE
    ) {
      app_expect_js(
        self, private,
        script = script, arguments = arguments,
        ...,
        file = file, timeout = timeout, pre_snapshot = pre_snapshot, cran = cran
      )
    },


    #' @description
    #' Expect a downloadable file
    #'
    #' Given a [shiny::downloadButton()]/[shiny::downloadLink()] `id`, the corresponding
    #' file will be downloaded and saved as a snapshot file.
    #'
    #' @param id Output id of [shiny::downloadButton()]/[shiny::downloadLink()]
    #' @param name File name to save file to (including file name extension). The default, `NULL`,
    #'   generates an ascending sequence of names: `001.download`,
    #'   `002.download`, etc.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/10_download", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #'
    #' # Save snapshot of rock.csv as 001.download
    #' # Save snapshot value of `rock.csv` to capture default file name
    #' app$expect_download("downloadData")
    #' }
    expect_download = function(id, ..., name = NULL, cran = FALSE) {
      app_expect_download(self, private, id = id, ..., name = name, cran = cran)
    },
    #' @description
    #' Get downloadable file
    #'
    #' Given a [shiny::downloadButton()]/[shiny::downloadLink()] `id`, the corresponding
    #' file will be downloaded and saved as a file.
    #'
    #' @param id Output id of [shiny::downloadButton()]/[shiny::downloadLink()]
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
    #' * Otherwise, a tempfile ending in `.download` will be returned.
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
    get_download = function(id, filename = NULL) {
      app_get_download(self, private, id = id, filename = filename)
    },


    #' @description
    #' Get a single `input`, `output`, or `export` value
    #'
    #' This is a helper function around `$get_values()` to retrieve a single
    #' `input`, `output`, or `export` value. Only a single `input`, `output`, or
    #' `export` value can be used.
    #'
    #' Note, values that contain environments or other values that will have
    #' trouble serializing may not work well. This includes R6 objects.
    #'
    #' @param input,output,export One of these variable should contain a single
    #'   string value. If more than one value is specified or no values are
    #'   specified, an error will be thrown.
    #' @return The requested `input`, `output`, or `export` value.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/04_mpg", package = "shiny")
    #' app <- AppDriver$new(app_path)
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
    #' trouble serializing may not work well. This includes R6 objects.
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
    #' Expect `input`, `output`, and `export` values
    #'
    #' A JSON snapshot is saved of given the results from the underlying call to `$get_values()`.
    #'
    #' Note, values that contain environments or other values that will have
    #' trouble serializing may not work well. This includes R6 objects. Instead,
    #' these objects should be manually inspected and have their components
    #' tested individually.
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
    #'     [`chromote::ChromoteSession`]'s `$screenshot()` method. The `selector`
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
    #' # Display in viewer
    #' app$screenshot()
    #'
    #' # Update bins then display `"disPlot"` in viewer
    #' app$set_inputs(bins = 10)
    #' app$screenshot(selector = "#distPlot")
    #'
    #' # Save screenshot to file and view it
    #' tmpfile <- tempfile(fileext = ".png")
    #' app$screenshot(tmpfile)
    #' showimage::show_image(tmpfile)
    #' }
    screenshot = function(
      file = NULL,
      ...,
      screenshot_args = missing_arg(),
      delay = missing_arg(),
      selector = missing_arg()
    ) {
      app_screenshot(
        self, private,
        file = file,
        ...,
        screenshot_args = screenshot_args,
        delay = delay,
        selector = selector
      )
    },
    #' @description
    #' Expect a screenshot of the Shiny application
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
    #'   [`chromote::ChromoteSession`]'s `$screenshot()` method. If missing, the
    #'   value will default to `$new(screenshot_args=)`.
    #'
    #' If the value is:
    #'   * `TRUE`: A screenshot of the whole page will be taken with no delay
    #'   * A named list of arguments: Arguments passed directly to
    #'     [`chromote::ChromoteSession`]'s `$screenshot()` method. The `selector`
    #'     and `delay` will default to `"html"` and `0` respectively.
    #'
    #' If `FALSE` is provided, the parameter will be ignored and a
    #' screenshot will be taken with default behavior.
    #' @param name The file name to be used for the snapshot. The file extension
    #'   will overwritten to `.png`. By default, the `name` supplied to
    #'   `app` on initialization with a counter will be used (e.g. `"NAME-001.png"`).
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path, variant = platform_variant())
    #'
    #' # Expect a full size screenshot
    #' app$expect_screenshot()
    #'
    #' # Very brittle test
    #' app$expect_screenshot(selector = "#distPlot")
    #' }
    expect_screenshot = function(
      ...,
      screenshot_args = missing_arg(),
      delay = missing_arg(),
      selector = missing_arg(),
      name = NULL,
      cran = FALSE
    ) {
      app_expect_screenshot_and_variant(
        self, private,
        ...,
        screenshot_args = screenshot_args,
        delay = delay,
        selector = selector,
        name = name,
        cran = cran
      )
    },

    #' @description Set input values
    #'
    #' Set Shiny inputs by sending the value to the Chrome browser and programaticly updating the values. Given `wait_ = TRUE`, the method will not return until an output value has been updated.
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
    #' cat(app$get_text("#view"))
    #' app$set_inputs(dataset = "cars", obs = 6)
    #' app$click("update")
    #' cat(app$get_text("#view"))
    #' }
    set_inputs = function(
      ...,
      wait_ = TRUE,
      timeout_ = 3 * 1000,
      allow_no_input_binding_ = FALSE,
      priority_ = c("input", "event")
    ) {
      app_set_inputs(
        self, private, ..., wait_ = wait_, timeout_ = timeout_,
        allow_no_input_binding_ = allow_no_input_binding_, priority_ = priority_
      )
    },

    #' @description Click an element
    #'
    #' Find a Shiny input/output value or DOM CSS selector and click it using
    #' the [DOM method
    #' `TAG.click()`](https://www.w3schools.com/jsref/met_html_click.asp).
    #'
    #' @param input,output,selector A name of an Shiny `input`/`output` value or
    #'   a DOM CSS selector. Only one of these may be used.
    #' @param ... If `input` is used, all extra arguments are passed to
    #'   `$set_inputs(!!input := "click", ...)`. By default, this means that the
    #'   `AppDriver` will wait until an output has been updated within the
    #'   specified `timeout_`.
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
    upload_file = function(..., wait_ = TRUE, timeout_ = 3 * 1000) {
      app_upload_file(self, private, ..., wait_ = wait_, timeout_ = timeout_)
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
    #' @param timeout How long the script has to return a `true`thy value, in ms.
    #' @param interval How often to check for the condition, in ms.
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
    #'   app$wait_for_js("return Math.floor((Date.now() / 1000) % 10) == 5;")
    #' )
    #'
    #' ## A second example where we run the contents of a JavaScript file
    #' ## and use the result to wait for a condition
    #' app$execute_js(file = "complicated_file.js")
    #' app$wait_for_js("return complicated_condition();")
    #' }
    wait_for_js = function(script, timeout = 30 * 1000, interval = 100) {
      app_wait_for_js(self, private, script = script, timeout = timeout, interval = interval)
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
    #' @param timeout How often to check for the condition, in ms.
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
    wait_for_idle = function(duration = 500, timeout = 30 * 1000) {
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
    #' @param timeout How long to wait (in ms) before throwing an error.
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
      timeout = 15 * 1000,
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


    #' @description
    #' Execute JavaScript code in the browser.
    #'
    #' This function will block the local R session until the code has finished
    #' executing its _tick_ in the browser. If a `Promise` is returned from the
    #' script, `$execute_js()` will wait for the promise to resolve. To have
    #' JavaScript code execute asynchronously, wrap the code in a Promise object
    #' and have the script return an atomic value.
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
    #' app$execute_js("return 1 + 1;")
    #' #> [1] 2
    #'
    #' # Execute a JavaScript Promise. Return the resolved value.
    #' app$execute_js("
    #'   return new Promise((resolve) => {
    #'     setTimeout(() => resolve(1 + 1), 1000)
    #'   }).
    #'   then((value) => value + 1);
    #' ")
    #' #> [1] 3
    #' }
    execute_js = function(
      script = missing_arg(),
      ...,
      arguments = list(),
      file = missing_arg(),
      timeout = 15 * 1000
    ) {
      app_execute_js(
        self, private,
        script = script,
        ...,
        arguments = arguments,
        file = file,
        timeout = timeout
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
    #' #> ℹ The following HTML id values are not unique:
    #' #> • text
    #'
    #' # Manually assert that all names are unique
    #' app <- AppDriver$new(shiny_app, check_names = FALSE)
    #' app$expect_unique_names()
    #' #> Error: `app_check_unique_names(self, private)` threw an unexpected warning.
    #' #> Message: ! Shiny inputs should have unique HTML id values.
    #' #> ℹ The following HTML id values are not unique:
    #' #>   • text
    #' #> Class:   rlang_warning/warning/condition
    #' }
    expect_unique_names = function() {
      app_expect_unique_names(self, private)
    },


    #' @description
    #' Retrieve the Shiny app path
    #'
    #' @return The directory containing the Shiny application or Shiny runtime
    #'   document.
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
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
    #' There are a few standard debug types that may be used:
    #' * `"shiny_console"`: Displays the console messages from the Shiny server when `$get_log()` is called.
    #' * `"browser"`: Displays the browser console messages when `$get_log()` is called.
    #' * `"shinytest2"`: Displays the messages saved by the `window.shinytest2` object in the browser when `$get_log()` is called.
    #' * `"ws_messages"`: Saves all messages sent by Shiny to the
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
    #'   * `"shiny"`: `"log"` or `"error"`; These two levels correspond to the
    #'     `stdin` or `stdout` messages.
    #'   * `"chromote"`: Correspond to any level of a JavaScript
    #'     `console.LEVEL()` function call. Typically, these are "log"` and
    #'     `"error"` but can include `"info"`, `"debug"`, and `"warn"`. If
    #'     `options(shiny.trace = TRUE)`, then the level will recorded as
    #'     `"websocket"`.
    #' @examples
    #' \dontrun{
    #' app <- AppDriver$new(system.file("examples/01_hello", package = "shiny"))
    #' app$get_log()
    #' # \{shinytest2\} R  info  11:15:20.11 Start AppDriver initialization
    #' # \{shinytest2\} R  info  11:15:20.11 Starting Shiny app
    #' # \{shinytest2\} R  info  11:15:20.99 Creating new chromote session
    #' # \{shinytest2\} R  info  11:15:21.14 Navigating to Shiny app
    #' # \{shinytest2\} R  info  11:15:21.27 Injecting shiny-tracer.js
    #' # \{chromote\}   JS info  11:15:21.28 shinytest2; jQuery not found
    #' # \{chromote\}   JS info  11:15:21.28 shinytest2; Loaded
    #' # \{shinytest2\} R  info  11:15:21.28 Waiting until Shiny app starts
    #' # \{chromote\}   JS info  11:15:21.35 shinytest2; jQuery found
    #' # \{chromote\}   JS info  11:15:21.35 shinytest2; Waiting for shiny session to connect
    #' # \{chromote\}   JS info  11:15:21.57 shinytest2; Connected
    #' # \{chromote\}   JS info  11:15:21.57 shinytest2; Ready
    #' # \{chromote\}   JS info  11:15:21.65 shinytest2; shiny:busy
    #' # \{shinytest2\} R  info  11:15:21.65 Shiny app started
    #' # \{chromote\}   JS info  11:15:21.88 shinytest2; shiny:idle
    #' # \{chromote\}   JS info  11:15:21.88 shinytest2; shiny:value distPlot
    #' # \{shiny\}      R  error ----------- Loading required package: shiny
    #' # \{shiny\}      R  error ----------- Running application in test mode.
    #' # \{shiny\}      R  error -----------
    #' # \{shiny\}      R  error ----------- Listening on http://127.0.0.1:42558
    #'
    #'
    #' # To capture all websocket traffic, set `options = list(shiny.trace = TRUE)`
    #' app <- AppDriver$new(
    #'   system.file("examples/01_hello", package = "shiny"),
    #'   options = list(shiny.trace = TRUE)
    #' )
    #' app$get_log() # (long output lines have been truncated)
    #' # \{shinytest2\} R  info      11:09:57.43 Start AppDriver initialization
    #' # \{shinytest2\} R  info      11:09:57.43 Starting Shiny app
    #' # \{shinytest2\} R  info      11:09:58.27 Creating new chromote session
    #' # \{shinytest2\} R  info      11:09:58.40 Navigating to Shiny app
    #' # \{shinytest2\} R  info      11:09:58.53 Injecting shiny-tracer.js
    #' # \{chromote\}   JS info      11:09:58.53 shinytest2; jQuery not found
    #' # \{chromote\}   JS info      11:09:58.53 shinytest2; Loaded
    #' # \{shinytest2\} R  info      11:09:58.54 Waiting until Shiny app starts
    #' # \{chromote\}   JS info      11:09:58.61 shinytest2; jQuery found
    #' # \{chromote\}   JS info      11:09:58.61 shinytest2; Waiting for shiny session to connect
    #' # \{chromote\}   JS websocket 11:09:58.73 send \{"method":"init","data":\{"bins":30,|truncated
    #' # \{chromote\}   JS websocket 11:09:58.78 recv \{"config":\{"workerId":"","sessionId":|truncated
    #' # \{chromote\}   JS info      11:09:58.78 shinytest2; Connected
    #' # \{chromote\}   JS info      11:09:58.78 shinytest2; Ready
    #' # \{chromote\}   JS websocket 11:09:58.85 recv \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' # \{chromote\}   JS websocket 11:09:58.85 recv \{"busy":"busy"\}
    #' # \{chromote\}   JS info      11:09:58.85 shinytest2; shiny:busy
    #' # \{chromote\}   JS websocket 11:09:58.86 recv \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' # \{chromote\}   JS websocket 11:09:58.86 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{shinytest2\} R  info      11:09:58.87 Shiny app started
    #' # \{shinytest2\} R  info      11:09:59.07 Setting inputs: 'bins'
    #' # \{chromote\}   JS websocket 11:09:59.08 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.08 recv \{"busy":"idle"\}
    #' # \{chromote\}   JS info      11:09:59.08 shinytest2; shiny:idle
    #' # \{chromote\}   JS websocket 11:09:59.08 recv \{"errors":\{\},"values":\{"distPlot":\{|truncated
    #' # \{chromote\}   JS info      11:09:59.08 shinytest2; shiny:value distPlot
    #' # \{chromote\}   JS info      11:09:59.08 shinytest2; inputQueue: adding bins
    #' # \{chromote\}   JS info      11:09:59.09 shinytest2; inputQueue: flushing bins
    #' # \{chromote\}   JS websocket 11:09:59.10 send \{"method":"update","data":\{"bins":20\}\}
    #' # \{chromote\}   JS websocket 11:09:59.11 recv \{"progress":\{"type":"binding",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.11 recv \{"busy":"busy"\}
    #' # \{chromote\}   JS info      11:09:59.11 shinytest2; shiny:busy
    #' # \{chromote\}   JS websocket 11:09:59.12 recv \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' # \{chromote\}   JS websocket 11:09:59.14 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.18 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.19 recv \{"busy":"idle"\}
    #' # \{chromote\}   JS info      11:09:59.19 shinytest2; shiny:idle
    #' # \{chromote\}   JS websocket 11:09:59.21 recv \{"errors":\{\},"values":\{"distPlot":\{|truncated
    #' # \{chromote\}   JS info      11:09:59.21 shinytest2; shiny:value distPlot
    #' # \{shinytest2\} R  info      11:09:59.21 Finished setting inputs. Timedout: FALSE
    #' # \{shinytest2\} R  info      11:09:59.21 Getting all values
    #' # \{shiny\}      R  error     ----------- Loading required package: shiny
    #' # \{shiny\}      R  error     ----------- Running application in test mode.
    #' # \{shiny\}      R  error     -----------
    #' # \{shiny\}      R  error     ----------- Listening on http://127.0.0.1:1505
    #' # \{shiny\}      R  error     ----------- SEND \{"config":\{"workerId":"","sessionId":|truncated
    #' # \{shiny\}      R  error     ----------- RECV \{"method":"init","data":\{"bins":30,|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"custom":\{"showcase-src":\{"srcref"|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"busy":"busy"\}
    #' # \{shiny\}      R  error     ----------- SEND \{"custom":\{"showcase-src":\{"srcref"|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"busy":"idle"\}
    #' # \{shiny\}      R  error     ----------- SEND \{"errors":\{\},"values":\{"distPlot":\{|truncated
    #' # \{shiny\}      R  error     ----------- RECV \{"method":"update","data":\{"bins":20\}\}
    #' # \{shiny\}      R  error     ----------- SEND \{"progress":\{"type":"binding",|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"busy":"busy"\}
    #' # \{shiny\}      R  error     ----------- SEND \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{shiny\}      R  error     ----------- SEND \{"busy":"idle"\}
    #' # \{shiny\}      R  error     ----------- SEND \{"errors":\{\},"values":\{"distPlot":\{|truncated
    #'
    #' # The log that is returned is a `data.frame()`.
    #' log <- app$get_log()
    #' tibble::glimpse(log)
    #' #> $ workerid  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    #' #> $ timestamp <dttm> 2022-03-16 11:09:57, 2022-03-16 11:09:57, 2022-03-16 11:09:…
    #' #> $ location  <chr> "shinytest2", "shinytest2", "shinytest2", "shinytest2", "shi…
    #' #> $ level     <chr> "info", "info", "info", "info", "info", "info", "info", "inf…
    #' #> $ message   <chr> "Start AppDriver initialization", "Starting Shiny app", "Cre…
    #'
    #' # It may be filtered to find desired logs
    #' subset(log, level == "websocket")
    #' # \{chromote\}   JS websocket 11:09:58.73 send \{"method":"init","data":\{"bins":30,|truncated
    #' # \{chromote\}   JS websocket 11:09:58.78 recv \{"config":\{"workerId":"","sessionId":|truncated
    #' # \{chromote\}   JS websocket 11:09:58.85 recv \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' # \{chromote\}   JS websocket 11:09:58.85 recv \{"busy":"busy"\}
    #' # \{chromote\}   JS websocket 11:09:58.86 recv \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' # \{chromote\}   JS websocket 11:09:58.86 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.08 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.08 recv \{"busy":"idle"\}
    #' # \{chromote\}   JS websocket 11:09:59.08 recv \{"errors":\{\},"values":\{"distPlot":\{|truncated
    #' # \{chromote\}   JS websocket 11:09:59.10 send \{"method":"update","data":\{"bins":20\}\}
    #' # \{chromote\}   JS websocket 11:09:59.11 recv \{"progress":\{"type":"binding",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.11 recv \{"busy":"busy"\}
    #' # \{chromote\}   JS websocket 11:09:59.12 recv \{"custom":\{"showcase-src":\{"srcref":|truncated
    #' # \{chromote\}   JS websocket 11:09:59.14 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.18 recv \{"recalculating":\{"name":"distPlot",|truncated
    #' # \{chromote\}   JS websocket 11:09:59.19 recv \{"busy":"idle"\}
    #' # \{chromote\}   JS websocket 11:09:59.21 recv \{"errors":\{\},"values":\{"distPlot":\{|truncated
    #' }
    get_log = function() {
      app_get_log(self, private)
    },

    #' @description
    #' Add a message to the \pkg{shinytest2} log.
    #' @param message Single message to store in log
    #' @examples
    #' \dontrun{
    #' app_path <- system.file("examples/01_hello", package = "shiny")
    #' app <- AppDriver$new(app_path)
    #' app$log_message("Setting bins to smaller value")
    #' app$set_inputs(bins = 10)
    #' app$get_log()
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
    stop = function() {
      app_stop(self, private)
    }
  )
)
