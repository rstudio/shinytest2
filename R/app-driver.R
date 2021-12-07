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
#' @param iotype Type of the Shiny component. \pkg{shinytest2} is able to find
#'   the component by their name, so this is only needed if you use the same name
#'   for an input and output component.
#' @importFrom R6 R6Class
#' @export
AppDriver <- R6Class(# nolint
  "AppDriver",
  private = list(
    chromote_session = "<chromote::ChromoteSession>",
    shiny_process = NULL, # `callr::r_bg()` object

    appshot_count = "<Count>",
    shiny_url = "<Url>",

    debug_types = NULL,
    browser_logs = list(),

    event_log = list(),

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
    #' @param path Path to a directory containing a Shiny app, i.e. a
    #'   single `app.R` file or a `server.R`-`ui.R` pair.
    #' @param load_timeout How long to wait for the app to load, in ms.
    #'   This includes the time to start R. Defaults to 5s when running
    #'   locally and 10s when running on CI. Maximum value is 10s.
    #' @param screenshot_args Default set of arguments to pass in to [`chromote::ChromoteSession`]'s
    #' `$screenshot()` method when taking screnshots within `$expect_appshot()`. To disable screenshots by default, set to `FALSE`.
    # ' @param phantomTimeout How long to wait when connecting to phantomJS
    # '  process, in ms
    #' @template variant
    #' @param name Prefix name to use when saving testthat snapshot files
    #' @param check_names Check if widget names are unique?
    #' @param debug Start the app in debugging mode? In debugging mode debug
    #'   messages are printed to the console. See [debug_types()] for more information.
    #' @param view Opens the Chromote Session  in an interactive browser tab once initialization.
    #' @param seed An optional random seed to use before starting the application.
    #'   For apps that use R's random number generator, this can make their
    #'   behavior repeatable.
    #' @param clean_logs Whether to remove the stdout and stderr logs when the
    #'   Shiny process object is garbage collected.
    #' @param shiny_args A list of options to pass to [shiny::runApp()].
    #' @param render_args Passed to `rmarkdown::run()` for interactive `.Rmd`s.
    #' @param options A list of [base::options()] to set in the driver's child
    #'   process.
    #' @importFrom callr process
    #' @importFrom rlang abort
    initialize = function(
      path = testthat::test_path("../../"),
      ...,
      load_timeout = NULL,
      screenshot_args = NULL,
      check_names = TRUE,
      name = NULL,
      variant = getOption("shinytest2.variant", platform_variant()),
      debug = c("none", "all", debug_types()),
      view = FALSE,
      # phantomTimeout = 5000,
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
        debug = debug,
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
    #' @description
    #' Returns a set of names for of all inputs, outputs, and exports
    #' that Shiny is currently aware of.
    #'
    #' @param input,output,export Either `TRUE` to return all
    #'   input/output/exported values, or `FALSE` to return no names.
    get_names = function(input = TRUE, output = TRUE, export = TRUE) {
      app_get_names(self, private, input, output, export)
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
    #' Find a Shiny binding and click it using the DOM method `TAG.click()`
    #' @param id The HTML ID of the element to click
    click = function(id, iotype = c("auto", "input", "output")) {
      app_click(self, private, id, iotype)
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
    #' @param expr A string containing JavaScript code. Will wait until the
    #'   condition returns `true`.
    #' @param timeout How often to check for the condition, in ms.
    #' @param interval How often to check for the condition, in ms.
    #' @return `TRUE` if expression evaluates to `true` without error, before
    #'   timeout. Otherwise returns `FALSE`.
    # TODO-barret-rename; `self$wait_for_js`? Seems too misleading. `self$wait_for_js_condition` seems too long
    # TODO-barret-rename; $wait_for_script
    wait_for_condition = function(expr, timeout = 3 * 1000, interval = 100) {
      app_wait_for_condition(self, private, expr = expr, timeout = timeout, interval = interval)
    },

    #' @description Wait for Shiny to not be busy
    #'
    #' Waits until Shiny is not busy, i.e. the reactive graph has finished
    #' updating. This is useful, for example, if you've resized the window with
    #' `$set_window_size()` and want to make sure all plot redrawing is complete
    #' before take a screenshot.
    #'
    #' While this function may return true, Shiny may not have fully stablized.
    #' It is best to use `TODO-barret-implement wait for stable`
    #' @param timeout How often to check for the condition, in ms.
    #' @param interval How often to check for the condition, in ms.
    #' @return `TRUE` if done before before timeout; `NA` otherwise.
    # TODO-barret-implement; Add `self$wait_for_stable()`; Remove `self$waitForIdle()`?
    wait_for_idle = function(timeout = 3 * 1000, interval = 100) {
      app_wait_for_idle(self, private, timeout = timeout, interval = interval)
    },

    #' @description Wait for a new Shiny value
    #'
    #' Waits until the `input` or `output` with name `name` is not one of
    #' `ignore`d values, or the timeout is reached.
    #'
    #' This function can be useful in helping determine if an application
    #' has initialized or finished processing a complex reactive situation.
    #' @param id Name of the Shiny binding
    #' @param ignore List of possible values to ignore when checking for
    #'   updates.
    #' @param timeout How often to check for the condition, in ms.
    #' @param interval How often to check for the condition, in ms.
    #' @return Newly found value
    wait_for_value = function(
      id,
      ignore = list(NULL, ""),
      iotype = c("input", "output", "export"),
      timeout = 10 * 1000,
      interval = 400
    ) {
      app_wait_for_value(
        self, private,
        id = id, ignore = ignore, iotype = iotype,
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
    #' Execute JavaScript code in the browser with an additional `resolve` and `reject` arguments.
    #'
    #' This function will block the local R session until one of the last two arguments (`resolve` and `reject`) are called.
    #'
    #' @param script JS to execute. `resolve` and `reject` arguments are added to the script call. To return control back to the R session, one of these methods must be called.
    #' @return Self, invisibly.
    execute_script_callback = function(script, arguments = list(), ..., timeout = 15 * 1000) {
      app_execute_script_callback(
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
    #' @param type Log type: `"all"`, `"shiny_console"`, `"browser"`,
    #'   or `"shinytest2"`.
    get_debug_log = function(type = c("all", debug_types())) {
      app_get_debug_log(self, private, type)
    },

    #' @description
    #' Enable/disable debugging messages
    #' @param enable If `TRUE`, all Shiny WebSocket messages are recorded.
    #' This can be useful for debugging, but can be considered a memory leak in the browser.
    enable_debug_log_messages = function(enable = TRUE) {
      app_enable_debug_log_messages(self, private, enable)
    },

    #' @description
    #' Retrieve event log.
    # TODO-barret-implement; Test this with a snapshot
    get_event_log = function() {
      app_get_event_log(self, private)
    },

    #' @description
    #' Add an event to log.
    #' @param event Event name
    #' @param ... Addition data to store for event
    log_event = function(event, ...) {
      app_log_event(self, private, event, ...)
    },


    #' @description Stop the Shiny application
    #' Stop the app, the terminate external R process that runs the app and
    #' the Chromote Session instance.
    stop = function() {
      app_stop(self, private)
    }

  )
)
