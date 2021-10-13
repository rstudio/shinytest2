#' Launch test event recorder for a Shiny app
#'
#' @param app A [ShinyDriver2()] object, or path to a Shiny
#'   application.
# ' @param load_mode A boolean that determines whether or not the resulting test
# '   script should be appropriate for load testing.
#' @param seed A random seed to set before running the app. This seed will also
#'   be used in the test script.
#' @param variant
#'   If not-`NULL`, results will be saved in
#'   _snaps/{variant}/{test.md}`, so `variant` must be a single
#'   string of alphanumeric characters suitable for use as a
#'   directory name.
#'
#'   You can variants to deal with cases where the snapshot output
#'   varies and you want to capture and test the variations.
#'   Common use cases include variations for operating system, R
#'   version, or version of key dependency.
#' @param loadTimeout Maximum time to wait for the Shiny application to load, in
#'   milliseconds. If a value is provided, it will be saved in the test script.
#' @param debug start the underlying [ShinyDriver()] in `debug`
#'   mode and print those debug logs to the R console once recording is
#'   finished. The default, `'shiny_console'`, captures and prints R
#'   console output from the recorded R shiny process. Any value that the
#'   `debug` argument in [ShinyDriver()] accepts may be used
#'   (e.g., `'none'` may be used to completely suppress the driver logs).
#' @param shinyOptions A list of options to pass to `runApp()`. If a value
#'   is provided, it will be saved in the test script.
#' @export
record_test <- function(
  app = ".",
  ...,
  # load_mode = FALSE,
  seed = NULL,
  variant = os_name_and_r_version(),
  loadTimeout = 10000,
  debug = "shiny_console",
  shinyOptions = list()
) {
  ellipsis::check_dots_empty()

  for (class_val in c("shiny.appobj", "ShinyDriver")) {
    if (inherits(app, class_val)) {
      abort(paste0("Recording tests for ", class_val, " objects is not supported."))
    }
  }

  if (is.character(app)) {
    if (grepl("^http(s?)://", app)) {
      abort("Recording tests for remote apps is not yet supported.")
    }

    path <- app_path(app, "app")$app

    # Rmds need a random seed
    if (is_rmd(path) && is.null(seed)) {
      seed <- floor(stats::runif(1, min = 0, max = 1e5))
    }

    app <- ShinyDriver2$new(path, seed = seed, loadTimeout = loadTimeout, shinyOptions = shinyOptions)
    on.exit({
      rm(app)
      gc()
    })
  }

  if (!inherits(app, "ShinyDriver2")) {
    abort("Unknown object type to record tests for. Must supply a `ShinyDriver2` object or file path")
  }

  # Get the URL for the app. Depending on what type of object `app` is, it may
  # require starting an app.
  url <- app$getUrl()

  # Are we running in RStudio? If so, we might need to fix up the URL so that
  # it's externally accessible.
  if (rstudioapi::isAvailable()) {
    if (rstudioapi::hasFun("translateLocalUrl")) {
      # If the RStudio API knows how to translate URLs, call it.
      url <- rstudioapi::translateLocalUrl(url, absolute = TRUE)
    } else if (identical(rstudioapi::versionInfo()$mode, "server")) {
      # Older versions of the RStudio API don't know how to translate URLs, so
      # we'll need to do it ourselves if we're in server mode. For example,
      # http://localhost:1234/ is translated to ../../p/1234/.
      url <- paste0("../../p/", gsub(".*:([0-9]+)\\/?", "\\1", url), "/")
    }
  }

  # Use options to pass value to recorder app
  withr::with_options(
    list(
      shinytest2.recorder.url  = url,
      shinytest2.app           = app,
      shinytest2.debug         = debug,
      # shinytest2.load.mode     = load_mode,
      shinytest2.load.timeout  = if (!missing(loadTimeout)) loadTimeout,
      shinytest2.seed          = seed,
      shinytest2.shiny.options = shinyOptions
    ),
    res <- shiny::runApp(system.file("internal", "recorder", package = "shinytest2"))
  )

  test_app_ <- function() {
    testthat::test_file(path = res$test_file)
  }

  if (is.null(res$test_file)) {
    # Quit without saving

  } else if (isTRUE(res$run)) {

    # Run the test script
    test_app_()

  } else {
    if (length(res$dont_run_reasons) > 0) {
      inform(c("Not running test script because", res$dont_run_reasons))
    }

    inform(sprintf(
      'After making changes to the test script, run it with:\n  testthat::test_file("%s")',
      test_app_()
    ))
  }

  invisible(res$test_file)
}


#' Register an input processor for the test recorder
#'
#' @description
#' `registerInputProcessor()` registers an input processor which will be used by
#' the test recorder. The input processor function should take one parameter,
#' `value`, and return a string of R code which returns the desired value.
#'
#' `getInputProcessors()` returns a named list of all registered input processor
#' functions.
#'
#' @param inputType The name of an input type, for example,
#'   `"mypkg.numberinput"`.
#' @param processor An input processor function.
#' @export
#' @keywords internal
registerInputProcessor <- function(inputType, processor) {
  if (!is.function(processor) || !identical(names(formals(processor)), "value")) {
    abort("`processor` must be a function that takes one parameter, `value`")
  }
  recorder_input_processors[[inputType]] <- processor
}

#' @rdname registerInputProcessor
#' @export
getInputProcessors <- function() {
  as.list(recorder_input_processors)
}

# This environment holds input processors registered by other packages on load.
recorder_input_processors <- new.env(parent = emptyenv())
