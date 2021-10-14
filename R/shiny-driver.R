#' Remote control a Shiny app running in a headless browser
#'
#' @description
#' This class starts a Shiny app in a new R session, along with a \pkg{chromote}
#' headless browser that can be used to simulate user actions. This provides
#' a full simulation of a Shiny app so that you can test user interactions
#' with a live app.
#'
#' @param iotype Type of the Shiny widget. Usually shinytest2 finds the widgets
#'   by their name, so this is only needed if you use the same name for an
#'   input and output widget.
#' @param timeout,timeout_ Amount of time to wait before giving up (milliseconds).
#' @param wait_ Wait until all reactive updates have completed?
#' @param name Name of a shiny widget.
#' @param css CSS selector to find an HTML element.
#' @param linkText Find `<a>` HTML elements based on exact `innerText`
#' @param partialLinkText Find `<a>` HTML elements based on partial `innerText`
#' @param xpath Find HTML elements using XPath expressions.
#' @param checkInterval How often to check for the condition, in ms.
#' @importFrom R6 R6Class
#' @export
ShinyDriver2 <- R6Class(
  "ShinyDriver2",

  public = list(
    #' @param path Path to a directory containing a Shiny app, i.e. a
    #'   single `app.R` file or a `server.R`-`ui.R` pair.
    #' @param loadTimeout How long to wait for the app to load, in ms.
    #'   This includes the time to start R. Defaults to 5s when running
    #'   locally and 10s when running on CI. Maximum value is 10s.
    #' @param screenshot Take screenshots for each snapshot?
    # ' @param phantomTimeout How long to wait when connecting to phantomJS
    # '  process, in ms
    #' @template variant
    #' @param checkNames Check if widget names are unique?
    #' @param debug Start the app in debugging mode? In debugging mode debug
    #'   messages are printed to the console.
    #' @param seed An optional random seed to use before starting the application.
    #'   For apps that use R's random number generator, this can make their
    #'   behavior repeatable.
    #' @param cleanLogs Whether to remove the stdout and stderr logs when the
    #'   Shiny process object is garbage collected.
    #' @param shinyOptions A list of options to pass to [shiny::runApp()].
    #' @param renderArgs Passed to `rmarkdown::run()` for interactive `.Rmd`s.
    #' @param options A list of [base::options()] to set in the driver's child
    #'   process.
    initialize = function(
      path = ".",
      ...,
      loadTimeout = NULL,
      screenshot = TRUE,
      checkNames = TRUE,
      name = NULL,
      variant = getOption("shinytest2.variant", os_name_and_r_version()),
      debug = c("none", "all", shinytest2::ShinyDriver2$debugLogTypes),
      # phantomTimeout = 5000,
      seed = NULL,
      cleanLogs = TRUE,
      shinyOptions = list(),
      renderArgs = NULL,
      options = list()
   ) {
      ellipsis::check_dots_empty()

      # TODO-barret
      sd2_initialize(
        self, private,
        path = path, loadTimeout = loadTimeout, checkNames = checkNames,
        name = name, variant = variant, debug = debug,
        # phantomTimeout = phantomTimeout,
        seed = seed, cleanLogs = cleanLogs,
        shinyOptions = shinyOptions, renderArgs = renderArgs, options = options)
    },


    #' @description
    #' Stop the app, the terminate external R process that runs the app and
    #' the phantomjs instance.
    stop = function() {
      "!DEBUG sd2_stop"

      if (private$state == "stopped")
        return(invisible(self))

      self$logEvent("Closing Chrome session")
      # private$web$delete()
      private$chromote_obj$close()

      # If the app is being hosted locally, kill the process.
      if (!is.null(private$shinyProcess)) {
        self$logEvent("Ending Shiny process")

        # Attempt soft-kill before hard-kill. This is a workaround for
        # https://github.com/r-lib/processx/issues/95
        # SIGINT quits the Shiny application, SIGTERM tells R to quit.
        # Unfortunately, SIGTERM isn't quite the same as `q()`, because
        # finalizers with onexit=TRUE don't seem to run.
        private$shinyProcess$signal(tools::SIGINT)
        private$shinyProcess$wait(500)
        private$shinyProcess$signal(tools::SIGTERM)
        private$shinyProcess$wait(250)
        private$shinyProcess$kill()
      }

      private$state <- "stopped"
      invisible(self)
    },

    #' @description
    #' Finds a widget and queries its value. See the `getValue()` method of
    #' [Widget2] for more details.
    getValue = function(name, iotype = c("auto", "input", "output")) {
      "!DEBUG sd2_getValue `name` (`iotype`)"
      self$findWidget(name, iotype)$getValue()
    },

    #' @description
    #' Finds a widget and sets its value. It's a shortcut for `findElement()`
    #' plus `setValue()`; see the [Widget2] documentation for more details.
    #'
    #' @param value New value.
    #' @return Self, invisibly.
    setValue = function(name, value, iotype = c("auto", "input", "output")) {
      "!DEBUG sd2_setValue `name`"
      self$findWidget(name, iotype)$setValue(value)
      invisible(self)
    },

    #' @description
    #' Find a widget and click it. It's a shortcut for `findElement()`
    #' plus `click()`; see the [Widget2] documentation for more details.
    click = function(name, iotype = c("auto", "input", "output")) {
      self$findWidget(name, iotype)$click()
    },

    #' @description
    #' Returns a named list of all inputs, outputs, and export values.
    #'
    #' @param input,output,export Either `TRUE` to return all
    #'   input/output/exported values, or a character vector of specific
    #'   controls.
    getAllValues = function(input = TRUE, output = TRUE, export = TRUE) {
      # TODO-barret
      sd2_getAllValues(self, private, input, output, export)
    },

    #' @description
    #' Sends the specified keys to specific HTML element. Shortcut for
    #' `findWidget()` plus `sendKeys()`.
    #' @param keys Keys to send to the widget or the app.
    # ' See [webdriver::key] for how to specific special keys.
    #' @return Self, invisibly.
    sendKeys = function(name, keys) {
      "!DEBUG sd2_sendKeys `name`"
      # TODO-barret
      self$findWidget(name)$sendKeys(keys)
      invisible(self)
    },

    #' @description
    #' Sets size of the browser window.
    #' @param width,height Height and width of browser, in pixels.
    #' @return Self, invisibly.
    setWindowSize = function(width, height) {
      "!DEBUG sd2_setWindowSize `width`x`height`"
      chromote_set_window_size(private$chromote_obj, width = width, height = height)
      invisible(self)
    },

    #' @description
    #' Get current size of the browser window, as list of integer scalars
    #'   named `width` and `height`.
    getWindowSize = function() {
      "!DEBUG sd2_getWindowSize"
      viewport <- private$chromote_obj$Page$getLayoutMetrics()$cssLayoutViewport
      list(
        width = viewport$clientWidth,
        height = viewport$clientHeight
      )
    },

    ## Debugging

    #' @description
    #' Query one or more of the debug logs.
    #' @param type Log type: `"all"`, `"shiny_console"`, `"browser"`,
    #'   or `"shinytest2"`.
    getDebugLog = function(type = c("all", ShinyDriver2$debugLogTypes)) {
      # TODO-barret
      sd2_getDebugLog(self, private, match.arg(type, several.ok = TRUE))
    },

    #' @description
    #' Enable/disable debugging messages
    #' @param enable New value.
    enableDebugLogMessages = function(enable = TRUE) {
      # TODO-barret
      sd2_enableDebugLogMessages(self, private, enable)
    },

    ## Event logging

    #' @description Add event to log.
    #' @param event Event name
    #' @param ... Addition data to store for event
    logEvent = function(event, ...) {
      # TODO-barret
      sd2_logEvent(self, private, event, ...)
    },

    #' @description Retrieve event log.
    getEventLog = function() {
      # TODO-barret
      sd2_getEventLog(self, private)
    },

    #' @description Get current url
    getUrl = function() {
      "!DEBUG sd2_getUrl"
      chromote_eval(private$chromote_obj, "window.location.href")$result$value
    },

    #' @description Get page title
    getTitle = function() {
      "!DEBUG sd2_getTitle"
      chromote_eval(private$chromote_obj, "document.title")$result$value
    },

    #' @description Get complete source of current page.
    getSource = function() {
      "!DEBUG sd2_getSource"
      chromote_eval(private$chromote_obj, "document.documentElement.outerHTML")$result$value
    },

    #' @description Return to previous page
    #' @return Self, invisibly.
    goBack = function() {
      "!DEBUG sd2_goBack"
      chromote_eval(private$chromote_obj, "history.back()")$result$value
      invisible(self)
    },

    #' @description Refresh the browser
    #' @return Self, invisibly.
    refresh = function() {
      "!DEBUG refresh"
      chromote_eval(private$chromote_obj, "location.reload()")$result$value
      invisible(self)
    },

    #' @description
    #' Takes a screenshot of the current page and writes it to a PNG file or
    #' shows on current graphics device.
    #' @param file File name to save the screenshot to. If `NULL`, then
    #'   it will be shown on the R graphics device.
    #' @param id If not-`NULL`, will take a screenshot of element with this id.
    #' @param parent If `TRUE`, will take screenshot of parent of `id`; this
    #'   is useful if you also want to capture the label attached to a Shiny
    #'   control.
    #' @param delay The amount of seconds to wait before taking a screenshot
    #' @return Self, invisibly.
    # takeScreenshotLegacy = function(file = NULL, id = NULL, parent = FALSE) {
    takeScreenshot = function(
      filename = NULL,
      ..., # ignored? Send to chromote?
      delay = 0,
      selector = "html",
      cliprect = NULL,
      region = c("content", "padding", "border", "margin"),
      expand = NULL,
      scale = 1,
      wait_ = TRUE
    ) {
      "!DEBUG sd2_takeScreenshot"
      stopifnot(isTRUE(wait_))
      delay <- delay %||% 0
      checkmate::assert_number(delay, lower = 0, finite = TRUE, null.ok = TRUE)

      # TODO-barret
      self$logEvent("Taking screenshot")
      path <- tempfile("st2-", fileext = ".png")

      if (delay > 0) {
        Sys.sleep(delay)
      }

      # TODO-future: implement `selector` usage. May have to go back to using `chromote_obj$screenshot()`
      screenshot_data <- private$chromote_obj$Page$captureScreenshot(format = "png")$data
      writeBin(
        jsonlite::base64_dec(screenshot_data),
        path
      )
      # private$chromote_obj$screenshot(
      #   filename = path,
      #   ...,
      #   delay = 5,
      #   selector = selector,
      #   cliprect = cliprect,
      #   region = region,
      #   expand = expand,
      #   scale = scale,
      #   wait_ = wait_
      # )

      # Fix up the PNG resolution header on windows
      if (is_windows()) {
        normalize_png_res_header(path)
      }

      if (!is.null(list2(...)$id)) {
        stop("TODO-barret")
        png <- png::readPNG(path)
        element <- self$findElement(paste0("#", id))
        if (parent) {
          element <- element$findElement(xpath = "..")
        }
        pos <- element$getRect()
        pos$x2 <- pos$x + pos$width
        pos$y2 <- pos$y + pos$height

        png <- png[pos$y:pos$y2, pos$x:pos$x2, ]
        png::writePNG(png, path)
      }

      if (is.null(filename)) {
        withr::local_par(list(bg = "grey90"))
        png <- png::readPNG(path)
        plot(as.raster(png))
      } else {
        # utils::str(list(
        #   path = path,
        #   file = filename
        # ))
        fs::file_copy(path, filename)
      }

      invisible(self)
    },

    #' @description
    #' Find an HTML element on the page, using a CSS selector, XPath expression,
    #' or link text (for `<a>` tags). If multiple elements are matched, only
    #' the first is returned.
    # ' @return A [webdriver::Element].
    findElement = function(css = NULL
      # , linkText = NULL, partialLinkText = NULL, xpath = NULL
    ) {
      "!DEBUG sd2_findElement '`css'"
      # private$web$findElement(css, linkText, partialLinkText, xpath)
      chromote_find_element(private$chromote_obj, css)
    },

    #' @description
    #' Find all elements matching CSS selection, xpath, or link text.
    # ' @return A list of [webdriver::Element]s.
    findElements = function(css = NULL
    # , linkText = NULL, partialLinkText = NULL, xpath = NULL
    ) {
      "!DEBUG sd2_findElements '`css %||% linkText %||% partialLinkText %||% xpath`'"
      chromote_find_elements(private$chromote_obj, css)
    },

    #' @description
    #' Waits until a JavaScript `expr`ession evaluates to `true` or the
    #' `timeout` is exceeded.
    #' @param expr A string containing JavaScript code. Will wait until the
    #'   condition returns `true`.
    #' @return `TRUE` if expression evaluates to `true` without error, before
    #'   timeout. Otherwise returns `FALSE`.
    waitFor = function(expr, checkInterval = 100, timeout = 3000) {
      "!DEBUG sd2_waitFor"
      chromote_wait_for_condition(private$chromote_obj, expr, timeout_ms = timeout, delay_ms = checkInterval)
    },

    #' @description
    #' Waits until Shiny is not busy, i.e. the reactive graph has finished
    #' updating. This is useful, for example, if you've resized the window with
    #' `setWindowSize()` and want to make sure all plot redrawing is complete
    #' before take a screenshot.
    #' @return `TRUE` if done before before timeout; `NA` otherwise.
    waitForShiny = function()  {
      # Shiny automatically sets using busy/idle events:
      # https://github.com/rstudio/shiny/blob/e2537d/srcjs/shinyapp.js#L647-L655
      # Details of busy event: https://shiny.rstudio.com/articles/js-events.html
      # private$web$waitFor()
      chromote_wait_for_condition(
        private$chromote_obj,
        "!$('html').first().hasClass('shiny-busy')",
        timeout = 3 * 1000,
        delay_ms = checkInterval
      )
    },

    #' @description
    #' Waits until the `input` or `output` with name `name` is not one of
    #' `ignore`d values, or the timeout is reached.
    #'
    #' This function can be useful in helping determine if an application
    #' has initialized or finished processing a complex reactive situation.
    #' @param ignore List of possible values to ignore when checking for
    #'   updates.
    waitForValue = function(name, ignore = list(NULL, ""), iotype = c("input", "output", "export"), timeout = 10000, checkInterval = 400) {
      # TODO-barret
      sd2_waitForValue(self, private, name = name, ignore = ignore, iotype = match.arg(iotype), timeout = timeout, checkInterval = checkInterval)
    },

    #' @description
    #' Lists the names of all input and output widgets
    #' @return A list of two character vectors, named `input` and `output`.
    listWidgets = function() {
      sd2_listWidgets(self, private)
    },

    #' @description
    #' Check if Shiny widget names are unique.
    checkUniqueWidgetNames = function() {
      # TODO-barret
      sd2_checkUniqueWidgetNames(self, private)
    },

    #' @description Execute JS code
    #' @param script JS to execute. If a JS Promise is returned, `$executeScript()` will wait for the promise to resolve before returning.
    #' @param ... Additional arguments to script.
    #' @return Result of the script.
    executeScript = function(script, ...) {
      # TODO-barret; chromote$Runtime$evaluate() is blocking for the JS _tick_
      # TODO-barret; incorporate `wait_` parameters to not wait for the _tick_ to finish
      "!DEBUG sd2_executeScript"
      script_info <- chromote_execute_script(
        private$chromote_obj,
        script,
        awaitPromise = TRUE,
        arguments = rlang::list2(...)
      )
      script_info$result$value
      # invisible(self)
    },

    #' @description Execute JS code asynchronously.
    #' @param script JS to execute. If a JS Promise is returned, `$executeScriptAsync()` will wait for the Promise to be resolved.
    #' @param ... Additional arguments to script.
    #' @return Self, invisibly.
    executeScriptCallback = function(script, ...) {
      # Make sure the window is the context when executing the script.
      "!DEBUG sd2_executeScriptAsync"
      chromote_execute_script(
        private$chromote_obj,
        script,
        awaitPromise = TRUE,
        arguments = rlang::list2(...)
      )
      invisible(self)
    },

    ## Main methods

    #' @description
    #' Finds the a Shiny input or output control.
    #' @return A [Widget2].
    findWidget = function(name, iotype = c("auto", "input", "output")) {
      sd2_findWidget(private$chromote_obj, name, match.arg(iotype))
    },

    #' @description
    #' It performs one or more update operations via the browser, thens
    #' waits for the specified output(s) to update. The test succeeds if
    #' all specified output widgets are updated before the `timeout`.
    #' For updates that involve a lot of computation, increase the timeout.
    #'
    #' @param output Name of output control to check.
    #' @param ... Name-value pairs used to update inputs.
    expectUpdate = function(
      output, ..., timeout = 3000,
      iotype = c("auto", "input", "output")
    ) {
      # TODO-barret
      sd2_expectUpdate(self, private, output, ..., timeout = timeout,
                       iotype = match.arg(iotype))
    },

    #' @description
    #' Sets input values.
    #' @param ... Name-value pairs, `name1 = value1, name2 = value2` etc.
    #'   Enput with name `name1` will be assigned value `value1`.
    #' @param allowInputNoBinding_ When setting the value of an input, allow
    #'   it to set the value of an input even if that input does not have
    #'   an input binding.
    #' @param priority_ Sets the event priority. For expert use only: see
    #'   <https://shiny.rstudio.com/articles/communicating-with-js.html#values-vs-events> for details.
    #' @param values_ If `TRUE`, will return final updated values of inputs.
    #' @return Returns updated values, invisibly.
    setInputs = function(
      ..., wait_ = TRUE, values_ = TRUE, timeout_ = 3000,
      allowInputNoBinding_ = FALSE, priority_ = c("input", "event")
    ) {
      # TODO-barret
      sd2_setInputs(self, private, ..., wait_ = wait_, values_ = values_,
                   timeout_ = timeout_, allowInputNoBinding_ = allowInputNoBinding_,
                   priority_ = priority_)
    },

    #' @description
    #' Uploads a file to a file input.
    #' @param ... Name-path pairs, e.g. `name1 = path1`. The file located at
    #' `path1` will be uploaded to file input with name `name1`.
    #' @param values_ If `TRUE`, will return final updated values of download
    #'   control.
    uploadFile = function(..., wait_ = TRUE, values_ = TRUE, timeout_ = 3000) {
      # TODO-barret
      sd2_uploadFile(self, private, ..., wait_ = wait_, values_ = values_,
                     timeout_ = timeout_)
    },

    #' @description
    # TODO-barret link `recordTest()`
    #' Download a snapshot. Generally, you should not call this function
    #' yourself; it will be generated by `recordTest()` as needed.
    #' @param path Directory to save snapshots.
    #' @param screenshot Take screenshots for each snapshot?
    snapshotInit = function(path, screenshot = TRUE) {
      sd2_snapshotInitLegacy(self, private, path, screenshot)
    },

    #' @description
    #' Snapshot a file download action. Generally, you should not call this
    #' function yourself; it will be generated by `recordTest()` as needed.
    #' @param id Output id of [shiny::downloadButton()]/[shiny::downloadLink()]
    #' @param filename File name to save file to. The default, `NULL`,
    #'   generates an ascending sequence of names: `001.download`,
    #'   `002.download`, etc.
    snapshotDownload = function(id, filename = NULL) {
      # TODO-barret
      sd2_snapshotDownload(self, private, id, filename)
    },

    #' @description Directory where app is located
    getAppDir = function() {
      # path can be a directory (for a normal Shiny app) or path to a .Rmd
      if (self$isRmd()) fs::path_dir(private$path) else private$path
    },

    #' @description App file name, i.e. `app.R` or `server.R`. `NULL` for Rmds.
    getAppFilename = function() {
      if (!self$isRmd()) {
        NULL
      } else {
        fs::path_file(private$path)
      }
    },

    #' @description Is this app an Shiny Rmd document?
    isRmd = function() {
      is_rmd(private$path)
    },

    #' @description
    #' Internal method. Generally, you should not call this function
    #' yourself; it will be generated by `recordTest()` as needed.
    #' @param items Elements to include in snapshot
    #' @param filename Filename to use
    #' @param screenshot Take a screenshot? Overrides value set by
    #'   `$snapshotInit()`
    expectSnapshot = function(
      ...,
      name = NULL,
      items = NULL,
      screenshot = NULL,
      error = FALSE,
      cran = FALSE
    ) {
      sd2_expectSnapshot(
        self, private, ..., name = name, items = items, screenshot = screenshot,
        cran = cran, error = error
      )
    },
    #' @description
    #' Internal method. Generally, you should not call this function
    #' yourself; it will be generated by `recordTest()` as needed.
    #' @param items Elements to include in snapshot
    #' @param filename Filename to use
    #' @param screenshot Take a screenshot? Overrides value set by
    #'   `$snapshotInit()`
    expectSnapshotJS = function(
      script,
      ...,
      post_script = NULL,
      cran = FALSE
    ) {
      sd2_expectSnapshotJS(
        self, private,
        script = script, arguments = list2(...),
        post_script = post_script, cran = cran
      )
    },
    #' @description Internal method. Retrieve temporary directory where snapshots are stored.
    getSnapshotDir = function() {
      private$snapshotDir
    },

    #' @description Deprecated.
    getRelativePathToApp = function() {
      abort("app$getRelativePathToApp() is no longer used")
    },
    #' @description Directory where tests are located
    getTestsDir = function() {
      abort("app$getTestsDir() is not longer used")
    },
    #' @description Deprecated.
    #' @param ... Ignored
    snapshotCompare = function(...) {
      # TODO-barret
      abort("app$snapshotCompare() is no longer used")
    }
  ),

  active = list(
    # TODO-barret; make regular public field
    # TODO-barret; rename to `chromote_session`
    # chromote_session = NULL,
    chromote = function(value) {
      if (!missing(value)) {
        stop("`$chromote` can only be read, not set")
      }
      private$chromote_obj
    }
  ),

  private = list(
    name = NULL, # character / NULL
    variant = NULL, # character / NULL

    state = "stopped",                  # stopped or running
    path = NULL,                        # Full path to app (including filename if it's a .Rmd)
    shinyUrlProtocol = NULL,            # "http" or "https"
    shinyUrlHost = NULL,                # usually 127.0.0.1
    shinyUrlPort = NULL,
    shinyUrlPath = NULL,
    shinyProcess = NULL,                # process object
    # phantomPort = NULL,
    # web = NULL,                         # webdriver session
    chromote_obj = NULL,                      # chromote object
    afterId = NULL,
    shinyTest2SnapshotBaseUrl = NULL,   # URL for shiny's test API
    snapshotDir = tempfile("st2-"),     # Directory for storing test artifacts
    snapshotCount = 0,
    snapshotScreenshot = TRUE,         # Whether to take screenshots for each snapshot
    shinyWorkerId = NA_character_,
    eventLog = list(),
    cleanLogs = TRUE,                  # Whether to clean logs when GC'd

    # Function run on garbage collection
    finalize = function() {
      # Stop the app
      self$stop()

      # Chromote has its own cleanup process on finalize

      if (isTRUE(private$cleanLogs)) {
        unlink(private$shinyProcess$get_output_file())
        unlink(private$shinyProcess$get_error_file())
      }

      # TODO-barret; Remove temp dir when garbage collected?
    },

    startShiny = function(
      path, seed = NULL, loadTimeout = 10000,
      shinyOptions = list(), renderArgs = NULL, options = list()
    ) {
      # TODO-barret
      sd2_startShiny(self, private, path, seed, loadTimeout, shinyOptions, renderArgs, options)
    },

    # TODO-barret; make private? No?
    getShinyUrl = function() {
      sd2_getShinyUrl(self, private)
    },

    # TODO-barret; make private? Yes?
    setShinyUrl = function(url) {
      sd2_setShinyUrl(self, private, url)
    },

    setupDebugging = function(debug) {
      # TODO-barret
      sd2_setupDebugging(self, private, debug)
    },

    queueInputs = function(...) {
      # TODO-barret
      sd2_queueInputs(self, private, ...)
    },

    flushInputs = function(wait = TRUE, timeout = 1000) {
      # TODO-barret
      sd2_flushInputs(self, private, wait, timeout)
    },

    getTestSnapshotUrl = function(
      input = TRUE, output = TRUE,
      export = TRUE, format = "json"
    ) {
      # TODO-barret
      sd2_getTestSnapshotUrl(self, private, input, output, export, format)
    }
  )
)

ShinyDriver2$debugLogTypes <- c(
  "shiny_console",
  "browser",
  "shinytest2"
)

sd2_stop <- function(self, private) {
  "!DEBUG sd2_stop"
  # TODO-barret; make sure function works. `$web` is not defined.

  if (private$state == "stopped")
    return(invisible(self))

  self$logEvent("Closing PhantomJS session")
  private$web$delete()

  # If the app is being hosted locally, kill the process.
  if (!is.null(private$shinyProcess)) {
    self$logEvent("Ending Shiny process")

    # Attempt soft-kill before hard-kill. This is a workaround for
    # https://github.com/r-lib/processx/issues/95
    # SIGINT quits the Shiny application, SIGTERM tells R to quit.
    # Unfortunately, SIGTERM isn't quite the same as `q()`, because
    # finalizers with onexit=TRUE don't seem to run.
    private$shinyProcess$signal(tools::SIGINT)
    private$shinyProcess$wait(500)
    private$shinyProcess$signal(tools::SIGTERM)
    private$shinyProcess$wait(250)
    private$shinyProcess$kill()
  }

  private$state <- "stopped"
  invisible(self)
}

sd2_waitForValue <- function(self, private, name, ignore = list(NULL, ""), iotype = "input", timeout = 10000, checkInterval = 400) {
  "!DEBUG sd2_waitForValue"

  timeoutSec <- as.numeric(timeout) / 1000
  if (!is.numeric(timeoutSec) || is.na(timeoutSec) || is.nan(timeoutSec)) {
    abort("timeout must be numeric")
  }
  checkInterval <- as.numeric(checkInterval)
  if (!is.numeric(checkInterval) || is.na(checkInterval) || is.nan(checkInterval)) {
    abort("checkInterval must be numeric")
  }

  now <- function() {
    as.numeric(Sys.time())
  }

  endTime <- now() + timeoutSec

  while (TRUE) {
    value <- try({
      # by default, do not retrieve anything
      args <- list(input = FALSE, output = FALSE, export = FALSE)
      # only retrieve `name` from `iotype`
      args[[iotype]] <- name
      do.call(self$getAllValues, args)[[iotype]][[name]]
    }, silent = TRUE)

    # if no error when trying ot retrieve the value..
    if (!inherits(value, "try-error")) {
      # check against all invalid values
      isInvalid <- vapply(ignore, identical, logical(1), x = value)
      # if no matches, then it's a success!
      if (!any(isInvalid)) {
        return(value)
      }
    }

    # if too much time has elapsed... throw
    if (now() > endTime) {
      abort(paste0("timeout reached when waiting for value: ", name))
    }

    # wait a little bit for shiny to do some work
    Sys.sleep(checkInterval / 1000)
  }
}

sd2_listWidgets <- function(self, private) {
  "!DEBUG sd2_listWidgets"
  res <- chromote_eval(private$chromote_obj,
    "shinytest2.listWidgets()"
  )$result$value

  res$input <- unlist(res$input)
  res$output <- unlist(res$output)
  res
}

sd2_checkUniqueWidgetNames <- function(self, private) {
  "!DEBUG sd2_checkUniqueWidgetNames"
  widgets <- self$listWidgets()
  inputs <- widgets$input
  outputs <- widgets$output

  check <- function(what, ids) {
    if (any(duplicated(ids))) {
      dup <- paste(unique(ids[duplicated(ids)]), collapse = ", ")
      warning("Possible duplicate ", what, " widget ids: ", dup)
    }
  }

  if (any(inputs %in% outputs)) {
    dups <- unique(inputs[inputs %in% outputs])
    warning(
      "Widget ids both for input and output: ",
      paste(dups, collapse = ", ")
    )

    ## Otherwise the following checks report it, too
    inputs <- setdiff(inputs, dups)
    outputs <- setdiff(outputs, dups)
  }

  if (length(inputs) > 0) check("input", inputs)
  if (length(outputs) > 0) check("output", outputs)
}
