#' @importFrom rlang missing_arg
NULL

#' Remote control a Shiny app running in a headless browser
#'
#' @description
#' This class starts a Shiny app in a new R session, along with a \pkg{chromote}
#' headless browser that can be used to simulate user actions. This provides
#' a full simulation of a Shiny app so that you can test user interactions
#' with a live app.
#'
#' @param ... Must be empty. Allows for parameter expansion.
#' @param arguments A list of unnamed arguments to send to the script.
#' @param timeout Amount of time to wait before giving up (milliseconds).
#' @param cran Should these expectations be verified on CRAN? By default,
#'        they are not, because snapshot tests tend to be fragile
#'        because they often rely on minor details of dependencies.
#' @param wait_ Wait until all reactive updates have completed?
#' @param timeout_ Amount of time to wait before giving up (milliseconds).
#' @param hash_images If `TRUE`, images will be hashed before being returned. Otherwise, all images will return their full data64 encoded value.
#' @param screenshot_args This named list of arguments is passed along to [`chromote::ChromoteSession`]'s `$screenshot()` method.
#'   If missing, the value will default to `$initialize(screenshot_args=)`.
#'
#' If the value is:
#'   * `TRUE`: A screenshot of the whole page will be taken with no delay
#'   * A named list of arguments: Arguments passed directly to [`chromote::ChromoteSession`]'s
#' `$screenshot()` method. The selector and delay will default to `"html"` and `0` respectively.
#'
#' If a `FALSE` value is provided, the parameter will be ignored and a screenshot will be taken with default behavior.
#' @param delay The number of milliseconds to wait before taking the screenshot. This value can either be supplied as `delay` or `screenshot_args`'s delay slot. The `delay` parameter will have preference.
#' @param selector The selector is a CSS selector that will be used to select a portion of the page to be captured. This value can either be supplied as `selector` or `screenshot_args`'s selector slot. The `selector` parameter will have preference.
#' The default value is to take a picture of the whole page.
#' @importFrom R6 R6Class
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

    path = NULL, # Full path to app (including filename if it's a .Rmd)
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
    #' Initialize the AppDriver
    #'
    #' @section Startup failure:
    #'
    #' If the app throws an error during initialization, the AppDriver will
    #' will be stored in `rlang::last_error()$app`. This allows for the "failure
    #' to initialize" to be signaled while also allowing for the `app` to be
    #' retrieved after any initialization error has been thrown.
    #'
    #' @param path Path to a directory containing a Shiny app, i.e. a
    #'   single `app.R` file or a `server.R`-`ui.R` pair.
    #' @param name Prefix name to use when saving testthat snapshot files
    #' @template variant
    #' @param seed An optional random seed to use before starting the application.
    #'   For apps that use R's random number generator, this can make their
    #'   behavior repeatable.
    #' @param load_timeout How long to wait for the app to load, in ms.
    #'   This includes the time to start R. Defaults to 10s when running
    #'   locally and 20s when running on CI.
    #' @param screenshot_args Default set of arguments to pass in to [`chromote::ChromoteSession`]'s
    #' `$screenshot()` method when taking screnshots within `$expect_screenshot()`. To disable screenshots by default, set to `FALSE`.
    #' @param expect_values_screenshot_args The value for `screenshot_args` when producing a debug screenshot for `$expect_values()`.
    #' @param check_names Check if widget names are unique once the application initially loads?
    #' @param view Opens the Chromote Session in an interactive browser tab once initialization. Defaults to `FALSE`.
    #' @param height,width Window size to use when opening the Chromote Session. These values will only be used if both `height` and `width` are not `NULL`.
    #' @param clean_logs Whether to remove the stdout and stderr logs when the
    #'   Shiny process object is garbage collected.
    #' @param shiny_args A list of options to pass to [shiny::runApp()]. Ex: `list(port = 8080)`.
    #' @param render_args Passed to `rmarkdown::run(render_args=)` for interactive `.Rmd`s. Ex: `list(quiet = TRUE)
    #' @param options A list of [base::options()] to set in the driver's child
    #'   process. See [`shiny::shinyOptions()`] for inspiration. If `shiny.trace = TRUE`,
    #'   then all WebSocket traffic will be captured by `chromote` and logged.
    #' @importFrom callr process
    #' @importFrom rlang abort
    initialize = function(
      path = testthat::test_path("../../"),
      ...,
      # TODO-barret-questions:
      # Should we have many options that can be set to override the defaults?
      # Like the shinytest2.variant? Or `shinytest2.seed`? Or even `shinytest2.idle.duration`?
      # Should `shinytest2.variant` be removed?
      name = NULL,
      variant = missing_arg(),
      seed = NULL,
      load_timeout = NULL,
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
        path = path,
        ...,
        load_timeout = load_timeout,
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
    #' Calls `$view()` on the Chromote Session object
    view = function() {
      app_view(self, private)
    },


    #' @description
    #' Expect snapshot of UI text
    #'
    #' `$expect_text()` will extract the text value of all matching elements via `TAG.textContent` and store them in a snapshot file.
    #' This method is more robust to internal package change as only the text values will be maintained.
    #'
    #' When possible, use `$expect_text()` over `$expect_html()` to allow package authors room to alter their HTML structures.
    #' The resulting array of `TAG.textContent` values found will be stored in a snapshot file.
    #'
    #' @param selector A DOM CSS selector to be passed into `document.querySelectorAll()`
    expect_text = function(selector, ..., cran = FALSE) {
      app_expect_text(self, private, selector, ..., cran = cran)
    },
    #' @param selector A DOM CSS selector to be passed into `document.querySelectorAll()`
    # TODO-barret-docs; Add note that this does not work for text inputs or text area inputs.
    get_text = function(selector) {
      app_get_text(self, private, selector = selector)
    },


    #' @description Expect snapshot of UI HTML
    #'
    #' `$expect_html()` will extract the full DOM structures of each matching element and store them in a snapshot file.
    #'
    #' @param selector A DOM selector to be passed into `document.querySelectorAll()`
    #' @param outer_html If `TRUE`, the full DOM structure will be returned (`TAG.outerHTML`).
    #'   If `FALSE`, the full DOM structure of the child elements will be returned (`TAG.innerHTML`).
    expect_html = function(selector, ..., outer_html = TRUE, cran = FALSE) {
      app_expect_html(self, private, selector, ..., outer_html = outer_html, cran = cran)
    },
    #' @param selector A DOM selector to be passed into `document.querySelectorAll()`
    #' @param outer_html If `TRUE`, the full DOM structure will be returned (`TAG.outerHTML`).
    #'   If `FALSE`, the full DOM structure of the child elements will be returned (`TAG.innerHTML`).
    # TODO-barret-docs; does not work with shadow DOM; Only works with updated HTML elements
    get_html = function(selector, ..., outer_html = TRUE) {
      app_get_html(self, private, selector, ..., outer_html = outer_html)
    },

    #' @description
    #' Expect snapshot of JS script output
    #'
    #' This is a building block function that should be called by other functions.
    #' For example, `$expect_text()` and `$expect_html()` are thin wrappers around this function.
    #'
    #' @param script A string containing the JS script to be executed.
    #' @param file A file containing JavaScript code to be read and used as the `script`. Only one of `script` or `file` can be specified.
    #' @param pre_snapshot A function to be called on the result of the script before taking the snapshot.
    #'   `$expect_html()` and `$expect_text()` both use [`unlist()`].
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
    #' Expect a download file action.
    #'
    #' Given a [shiny::downloadButton()]/[shiny::downloadLink()] `id`, the corresponding
    #' file will be downloaded and saved as a snapshot file.
    #'
    #' @param id Output id of [shiny::downloadButton()]/[shiny::downloadLink()]
    #' @param name File name to save file to. The default, `NULL`,
    #'   generates an ascending sequence of names: `001.download`,
    #'   `002.download`, etc.
    expect_download = function(id, ..., name = NULL, cran = FALSE) {
      app_expect_download(self, private, id = id, ..., name = name, cran = cran)
    },
    #' @description
    #' Retrieve download from a download file action.
    #'
    #' Given a [shiny::downloadButton()]/[shiny::downloadLink()] `id`, the corresponding
    #' file will be downloaded and saved as a file.
    #'
    #' @param id Output id of [shiny::downloadButton()]/[shiny::downloadLink()]
    #' @param filename File path to save the downloaded file to. If `NULL`, then a temp file ending in `.download` will be used.
    get_download = function(id, filename = NULL) {
      app_get_download(self, private, id = id, filename = filename)
    },


    #' @description
    #' Returns a named list of all inputs, outputs, and export values.
    #'
    #' @param input,output,export One of these variable should contain a single string value. If more than one value is specified or no values are specified, an error will be thrown.
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
    #' Returns a named list of all inputs, outputs, and export values.
    #'
    #' @param input,output,export Either `TRUE` to return all
    #'   input/output/exported values, or a character vector of specific
    #'   controls.
    # TODO-barret-docs; Add note about complex objects may have serialization issues.
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
    #' Take and appshot of the Shiny application
    #'
    #' Appshot: Shiny **App**lication Snap**shot**
    #'
    #' An appshot currently consists of two snapshot files:
    #' 1. A screenshot of the Shiny application using the `$screenshot()` function of the [`chromote::ChromoteSession`]
    #' 2. A JSON snapshot of all Shiny component values
    #'
    #' @param name The file name to be used for the snapshot. The file extension will be ignored. By default, this uses the name supplied to `app` on initialization with a counter.
    #' @param input,output,export
    #'   Depending on which parameters are supplied, different return values can occur:
    #'     * If `input`, `output`, and `export` are missing, then all values are included in the snapshot.
    #'     * If at least one `input`, `output`, or `export` is specified, then only those values are included in the snapshot.
    #'   The values supplied can be:
    #'     * A character vector of specific names to only include in the snapshot.
    #'     * `TRUE`, then all values of that type are included in the snapshot.
    #'     * Anything else will result in the parameter being ignored.
    #' @param screenshot_args This value is passed along to `$expect_screenshot()` where the resulting expectation is ignored. If missing, the default value will be `$initialize(expect_values_screenshot_args=)`.
    #'
    #'   The final value can either be:
    #'   * `TRUE`: A screenshot of the whole page will be taken with no delay
    #'   * `FALSE`: No screenshot will be taken
    #'   * A named list of arguments: Arguments passed directly to [`chromote::ChromoteSession`]'s
    #' `$screenshot()` method. The selector and delay will default to `"html"` and `0` respectively.
    # TODO-barret-docs; Examples!
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
    #' Take a screenshot of the Shiny application
    #'
    #' @param file If `NULL`, then the image will be displayed to the current Graphics Device. If a file path, then the screenshot will be saved to that file.
    # TODO-barret-docs; Examples!
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
    #' Expect a screenshot of the Shiny application
    #'
    #' @param screenshot_args This named list of arguments is passed along to
    #'   [`chromote::ChromoteSession`]'s `$screenshot()` method. If missing, the
    #'   value will default to `$initialize(screenshot_args=)`.
    #'
    #' If the value is:
    #'   * `TRUE`: A screenshot of the whole page will be taken with no delay
    #'   * A named list of arguments: Arguments passed directly to [`chromote::ChromoteSession`]'s
    #' `$screenshot()` method. The selector and delay will default to `"html"` and `0` respectively.
    #'
    #' If a `FALSE` value is provided, the parameter will be ignored and a
    #' screenshot will be taken with default behavior.
    #' @param delay The number of milliseconds to wait before taking the
    #'   screenshot. This value can either be supplied as `delay` or
    #'   `screenshot_args`'s delay slot. The `delay` parameter will have
    #'   preference.
    #' @param selector The selector is a CSS selector that will be used to
    #'   select a portion of the page to be captured. This value can either be
    #'   supplied as `selector` or `screenshot_args`'s selector slot. The
    #'   `selector` parameter will have preference. The default value is to take
    #'   a picture of the whole page.
    #' @param name The file name to be used for the snapshot. The file extension will overwritten to `.png`. By default, this uses the name supplied to `app` on initialization with a counter.
    # TODO-barret-docs; Examples!
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

    #' @description Set input values.
    #' @param ... Name-value pairs, `component_name_1 = value_1, component_name_2 = value_2` etc.
    #'   Input with name `component_name_1` will be assigned value `value_1`.
    #' @param allow_no_input_binding_ When setting the value of an input, allow
    #'   it to set the value of an input even if that input does not have
    #'   an input binding.
    #' @param priority_ Sets the event priority. For expert use only: see
    #'   <https://shiny.rstudio.com/articles/communicating-with-js.html#values-vs-events> for details.
    #' @return Returns updated values, invisibly.
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

    #' @description
    #' Find a Shiny input/output value or DOM CSS selector and click it using the DOM method `TAG.click()`
    #' @param input,output,selector A name of an Shiny input/output value or a DOM CSS selector. Only one of these may be used.
    #' @param ... If `input` is used, all extra arguments are passed to `$set_inputs(!!input := "click", ...)`. By default, this means that the AppDriver will wait until an output has been updated within the specified `timeout_`.
    click = function(
      input = missing_arg(), output = missing_arg(), selector = missing_arg(),
      ...
    ) {
      app_click(self, private, input = input, output = output, selector = selector, ...)
    },

    #' @description
    #' Uploads a file to a file input.
    #' @param ... Name-path pair, e.g. `component_name = file_path`. The file located at
    #' `file_path` will be uploaded to file input with name `component_name`.
    upload_file = function(..., wait_ = TRUE, timeout_ = 3 * 1000) {
      app_upload_file(self, private, ..., wait_ = wait_, timeout_ = timeout_)
    },


    #' @description Wait for a JavaScript expression to be true
    #'
    #' Waits until a JavaScript `expr`ession evaluates to `true` or the
    #' `timeout` is exceeded.
    #'
    #' @param script A string containing JavaScript code. This code must eventually return a `true`thy value or a `timeout` error will be thrown.
    #' @param timeout How long the script has to return a `true`thy value, in ms.
    #' @param interval How often to check for the condition, in ms.
    #' @return `invisible(self)` if expression evaluates to `true` without error within the timeout.
    #'   Otherwise an error will be thrown
    # TODO-barret-docs; Document $execute_js(file = "complicated_file.js"); $wait_for_js("return complicated_condition()")
    wait_for_js = function(script, timeout = 30 * 1000, interval = 100) {
      app_wait_for_js(self, private, script = script, timeout = timeout, interval = interval)
    },

    #' @description Wait for Shiny to not be busy (idle) for a set amount of time
    #'
    #' Waits until Shiny has not been busy for a set duration of time, i.e. no reactivity is updating or has occured.
    #' This is useful, for example, when waiting for your application to initialize or
    #' if you've resized the window with `$set_window_size()` and want to make sure all
    #' plot redrawing is complete before take a screenshot.
    #' @param duration How long Shiny must be idle (in ms) before unblocking the R session.
    #' @param timeout How often to check for the condition, in ms.
    #' @return `invisible(self)` if Shiny stablizes within the timeout. Otherwise an error will be thrown
    wait_for_idle = function(duration = 500, timeout = 30 * 1000) {
      app_wait_for_idle(self, private, duration = duration, timeout = timeout)
    },

    #' @description Wait for a new Shiny value
    #'
    #' Waits until `input`, `output`, or `export`ed shiny value is not one of
    #' `ignore`d values, or the timeout is reached.
    #'
    #' Only a single `input`, `output`, or `export` value may be used.
    #'
    #' This function can be useful in helping determine if an application
    #' has finished processing a complex reactive situation.
    #' @param input,output,export A name of an input, output, or export value. Only one of these parameters may be used.
    #' @param ignore List of possible values to ignore when checking for
    #'   updates.
    #' @param timeout How long we can wait (in ms) before throwing an error.
    #' @param interval How often to check for the condition, in ms.
    #' @return Newly found value
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
    #' This function will block the local R session until the code has finished executing its _tick_ in the browser.
    #' If a `Promise` is returned from the script, `$execute_js()` will wait for the promise to resolve.
    #' To have JavaScript code execute asynchronously, wrap the code in a Promise object and have the script return an atomic value.
    #' @param script JS to execute. If a JS Promise is returned, the R session will block until the promise has been resolved and return the value.
    #' @param file A (local) file containing JavaScript code to be read and used as the `script`. Only one of `script` or `file` can be specified.
    #' @return Result of the `script` (or `file` contents)
    # TODO-barret-docs; Document how they should make a promise and return NULL instead?
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
    #' Retrieve the Shiny app path
    #'
    #' @return If it's a .Rmd file, it will return the full .Rmd path, otherwise it will return the directory containing the file.
    get_path = function() {
      private$path
    },
    #' @description
    #' Retrieve the Shiny app URL
    #'
    #' @return URL
    get_url = function() {
      private$shiny_url$get()
    },

    #' @description
    #' Get current size of the browser window, as list of integer scalars
    #'   named `width` and `height`.
    get_window_size = function() {
      app_get_window_size(self, private)
    },
    #' @description
    #' Sets size of the browser window.
    #' @param width,height Height and width of browser, in pixels.
    #' @param wait If `TRUE`, `$wait_for_idle()` will be called after setting the window size.
    #'   This will allow for any width specific items (such as plots) to be rerendered.
    set_window_size = function(width, height, wait = TRUE) {
      app_set_window_size(self, private, width = width, height = height, wait = wait)
    },

    #' @description
    #' Chromote Session object from the \pkg{chromote} package.
    get_chromote_session = function() {
      app_get_chromote_session(self, private)
    },
    #' @description
    #' Get the variant supplied at initialization
    get_variant = function() {
      app_get_variant(self, private)
    },

    #' @description
    #' Query one or more of the debug logs.
    # TODO-barret-docs; show example of filtering output on type
    #' There are a few standard debug types that may be used:
    #' * `"shiny_console"`: Displays the console messages from the Shiny server when `$get_log()` is called.
    #' * `"browser"`: Displays the browser console messages when `$get_log()` is called.
    #' * `"shinytest2"`: Displays the messages saved by the `window.shinytest2` object in the browser when `$get_log()` is called.
    #' * `"ws_messages"`: Saves all messages sent by Shiny to the
    #' @return A data.frame with the following columns:
    #' * `workerid`: The shiny worker ID found within the browser
    #' * `timestamp`: POSIXct timestamp of the message
    #' * `location`: The location of the message was found. One of three values:
    #'   * `shinytest2`: Occurs when `$log_message()` is called
    #'   * `shiny`: Stdin and stdout messages from the Shiny server. Note `message()` output is sent to stdout.
    #'   * `chromote`: Captured by the \pkg{chromote} event handlers. See
    #'      [console API](https://chromedevtools.github.io/devtools-protocol/1-3/Runtime/#event-consoleAPICalled),
    #'      [exception thrown](https://chromedevtools.github.io/devtools-protocol/1-3/Runtime/#event-exceptionThrown),
    #'      [websocket sent](https://chromedevtools.github.io/devtools-protocol/1-3/Network/#event-webSocketFrameSent), and
    #'      [websocket received](https://chromedevtools.github.io/devtools-protocol/1-3/Network/#event-webSocketFrameReceived)
    #'      for more details
    #' * `level`:
    #' @examples
    #' \dontrun{
    #'
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
    #' }
    get_log = function() {
      app_get_log(self, private)
    },

    #' @description
    #' Add an message to log.
    #' @param message Single message to store in log
    log_message = function(message) {
      app_log_message(self, private, message = message)
    },


    #' @description Stop the Shiny application
    #' Stop the app, the terminate external R process that runs the app and
    #' the Chromote Session instance.
    #'
    #' To stop your shiny application and return a value from `$stop()`, see [`shiny::stopApp()`]. This is useful in testing to return context information.
    #'
    #' Typically, this is paired with a button that when clicked will call `shiny::stopApp(info)` to return `info` from the test app back to the main R session.
    #' @return The result of the background process if the Shiny application has already been terminated.
    stop = function() {
      app_stop(self, private)
    }

  )
)
