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
#' @importFrom R6 R6Class
#' @export
AppDriver <- R6Class(# nolint
  "AppDriver",
  private = list(
    chromote_session = "<chromote::ChromoteSession>",
    shiny_process = NULL, # `callr::r_bg()` object

    appshot_count = "<Count>",
    shiny_url = "<Url>",

    log = list(), # List of all log messages added via `$log_message()`

    clean_logs = TRUE, # Whether to clean logs when GC'd

    name = NULL, # character / NULL
    variant = NULL, # character / NULL
    state = "stopped", # "stopped" or "running"
    shiny_worker_id = NA_character_,

    path = NULL, # Full path to app (including filename if it's a .Rmd)
    appshot_dir = NULL, # Temp folder to store snapshot outputs
    default_screenshot_args = NULL, # Default screenshot args to use
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
    #' @param load_timeout How long to wait for the app to load, in ms.
    #'   This includes the time to start R. Defaults to 10s when running
    #'   locally and 20s when running on CI.
    #' @param screenshot_args Default set of arguments to pass in to [`chromote::ChromoteSession`]'s
    #' `$screenshot()` method when taking screnshots within `$expect_appshot()`. To disable screenshots by default, set to `FALSE`.
    # ' @param phantomTimeout How long to wait when connecting to phantomJS
    # '  process, in ms
    #' @template variant
    #' @param name Prefix name to use when saving testthat snapshot files
    #' @param check_names Check if widget names are unique?
    #' @param view Opens the Chromote Session in an interactive browser tab once initialization. Defaults to `FALSE`.
    #' @param seed An optional random seed to use before starting the application.
    #'   For apps that use R's random number generator, this can make their
    #'   behavior repeatable.
    #' @param clean_logs Whether to remove the stdout and stderr logs when the
    #'   Shiny process object is garbage collected.
    #' @param shiny_args A list of options to pass to [shiny::runApp()].
    #' @param render_args Passed to `rmarkdown::run()` for interactive `.Rmd`s.
    #' @param options A list of [base::options()] to set in the driver's child
    #'   process. See [`shiny::shinyOptions()`] for inspiration. If `shiny.trace`
    #'   is set to `TRUE`, then all WebSocket traffic will be captured by `chromote`
    #'   as to have access to the when the message was received by the browser.
    #' @importFrom callr process
    #' @importFrom rlang abort
    initialize = function(
      path = testthat::test_path("../../"),
      ...,
      load_timeout = NULL,
      variant = getOption("shinytest2.variant", platform_variant()),
      screenshot_args = NULL,
      check_names = TRUE,
      name = NULL,
      view = missing_arg(),
      seed = NULL,
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
        screenshot_args = screenshot_args,
        check_names = check_names,
        name = name,
        variant = variant,
        view = view,
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
    #' Take and appshot of the Shiny application
    #'
    #' Appshot: Shiny **App**lication Snap**shot**
    #'
    #' An appshot currently consists of two snapshot files:
    #' 1. A screenshot of the Shiny application using the `$screenshot()` function of the [`chromote::ChromoteSession`]
    #' 2. A JSON snapshot of all Shiny component values
    #'
    #' @param name The prefix name to be used for the snapshot. By default, this uses the name supplied to `app` on initialization.
    #' @param items Components to only be included in the snapshot. If supplied, can contain `inputs`, `output`, and `export`. Each value of `items` can either be `TRUE` (for all values) or a character list of names to use.
    #' @param screenshot If the value is `NULL`, then the initalization value of `screenshot_args` will be used. If this value is `NULL`, then `screenshot` will be set to the result of `!is.null(items)`.
    #'
    #'   The final value can either be:
    #'   * `TRUE`: A screenshot of the whole page will be taken with no delay
    #'   * `FALSE`: No screenshot will be taken
    #'   * A named list of arguments: Arguments passed directly to [`chromote::ChromoteSession`]'s
    #' `$screenshot()` method. The selector and delay will default to `"html"` and `0` respectively.
    expect_appshot = function(..., items = NULL, screenshot = NULL, name = NULL, cran = FALSE) {
      app_expect_appshot(
        self, private,
        ...,
        name = name, items = items, screenshot_args = screenshot, cran = cran
      )
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


    #' @description Expect snapshot of UI HTML
    #'
    #' `$expect_html()` will extract the full DOM structures of each matching element and store them in a snapshot file.
    #'
    #' @param selector A DOM selector to be passed into `document.querySelectorAll()`
    #' @param outer_html If `TRUE`, the full DOM structure will be returned (`TAG.outerHTML`).
    #'   If `FALSE`, the full DOM structure of the child elements will be returned (`TAG.innerHTML`).
    expect_html = function(selector, ..., outer_html = FALSE, cran = FALSE) {
      app_expect_html(self, private, selector, ..., outer_html = outer_html, cran = cran)
    },

    #' @description
    #' Expect snapshot of JS script output
    #'
    #' This is a building block function that should be called by other functions.
    #' For example, `$expect_text()` and `$expect_html()` are thin wrappers around this function.
    #'
    #' @param script A string containing the JS script to be executed.
    #' @param pre_snapshot A function to be called on the result of the script before taking the snapshot.
    #'   `$expect_html()` and `$expect_text()` both use [`unlist()`].
    #' TODO-barret-implement; $expect_js(script="TEXT")
    #' TODO-barret-implement; $expect_js(file = "file_path")
    expect_script = function(script, arguments = list(), ..., timeout = 15 * 1000, pre_snapshot = NULL, cran = FALSE) {
      app_expect_script(
        self, private,
        script = script, arguments = arguments,
        ...,
        timeout = timeout, pre_snapshot = pre_snapshot, cran = cran
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
    #' Returns a named list of all inputs, outputs, and export values.
    #'
    #' @param input,output,export Either `TRUE` to return all
    #'   input/output/exported values, or a character vector of specific
    #'   controls.
    get_values = function(input = TRUE, output = TRUE, export = TRUE) {
      app_get_values(self, private, input, output, export)
    },

    #' @description Set input values.
    #' @param ... Name-value pairs, `component_name_1 = value_1, component_name_2 = value_2` etc.
    #'   Input with name `component_name_1` will be assigned value `value_1`.
    #' @param allow_input_no_binding_ When setting the value of an input, allow
    #'   it to set the value of an input even if that input does not have
    #'   an input binding.
    #' @param priority_ Sets the event priority. For expert use only: see
    #'   <https://shiny.rstudio.com/articles/communicating-with-js.html#values-vs-events> for details.
    #' @param values_ If `TRUE`, will return final updated values of inputs.
    #' @return Returns updated values, invisibly.
    set_inputs = function(
      ...,
      wait_ = TRUE,
      values_ = TRUE,
      timeout_ = 3 * 1000,
      allow_input_no_binding_ = FALSE,
      priority_ = c("input", "event")
    ) {
      app_set_inputs(
        self, private, ..., wait_ = wait_, values_ = values_, timeout_ = timeout_,
        allow_input_no_binding_ = allow_input_no_binding_, priority_ = priority_
      )
    },

    #' @description
    #' Find a Shiny input/output value or DOM CSS selector and click it using the DOM method `TAG.click()`
    #' @param input,output,selector A name of an Shiny input/output value or a DOM CSS selector. Only one of these may be used.
    click = function(input = missing_arg(), output = missing_arg(), selector = missing_arg()) {
      app_click(self, private, input = input, output = output, selector = selector)
    },

    #' @description
    #' Uploads a file to a file input.
    #' @param ... Name-path pair, e.g. `component_name = file_path`. The file located at
    #' `file_path` will be uploaded to file input with name `component_name`.
    #' @param values_ If `TRUE`, will return final updated values of download
    #'   control. Otherwise, the return value will be `NULL`.
    upload_file = function(..., wait_ = TRUE, values_ = TRUE, timeout_ = 3 * 1000) {
      app_upload_file(self, private, ..., wait_ = wait_, values_ = values_, timeout_ = timeout_)
    },



    #' @description Wait for a JavaScript expression to be true
    #'
    #' Waits until a JavaScript `expr`ession evaluates to `true` or the
    #' `timeout` is exceeded.
    #' @param script A string containing JavaScript code. Will wait until the
    #'   condition returns `true`.
    #' @param timeout How often to check for the condition, in ms.
    #' @param interval How often to check for the condition, in ms.
    #' @return `invisible(self)` if expression evaluates to `true` without error within the timeout.
    #'   Otherwise an error will be thrown
    # TODO-barret-implement; $wait_for_js(script = , {local}file = )
    wait_for_script = function(script, timeout = 3 * 1000, interval = 100) {
      app_wait_for_script(self, private, script = script, timeout = timeout, interval = interval)
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
    #' @param input,output,export A name of an input, output, or export value. Only one of these may be used.
    #' @param ignore List of possible values to ignore when checking for
    #'   updates.
    #' @param timeout How long we can wait (in ms) before throwing an error.
    #' @param interval How often to check for the condition, in ms.
    #' @return Newly found value
    wait_for_value = function(
      input = missing_arg(),
      output = missing_arg(),
      export = missing_arg(),
      ...,
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
    #' If a `Promise` is returned from the script, `$execute_script()` will wait for the promise to resolve.
    #' To have JavaScript code execute asynchronously, wrap the code in a Promise object and have the script return an atomic value.
    #' @param script JS to execute. If a JS Promise is returned, `$execute_script()` will wait for the promise to resolve before returning.
    #' @return Result of the script.
    # TODO-barret; incorporate `wait_` parameters to not wait for the _tick_ to finish
    # TODO-barret-answer; Document how they should make a promise and return NULL instead?
    # @param script JS to execute. `resolve` and `reject` arguments are added to the script call. To return control back to the R session, one of these methods must be called.
    # TODO-barret-implement; execute_js(script = , {local}file = )
    execute_script = function(script, arguments = list(), ..., timeout = 15 * 1000) {
      app_execute_script(
        self, private,
        script = script,
        arguments = arguments,
        ...,
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
    set_window_size = function(width, height) {
      app_set_window_size(self, private, width, height)
    },

    #' @description
    #' Chromote Session object from the \pkg{chromote} package.
    get_chromote_session = function() {
      app_get_chromote_session(self, private)
    },

    #' @description
    #' Query one or more of the debug logs.
    # TODO-barret; show example of filtering output on type
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
    stop = function() {
      app_stop(self, private)
    }

  )
)
