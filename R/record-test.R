# TODO-barret-document; add_dont_run_reason("An uploadFile() must be updated: use the correct path relative to the app's ./tests/testthat directory, or copy the file to the app's ./tests/testthat directory.")
    # rlang::inform(c(
    #   "After making changes to the test script, run it with:",
    #   " " = paste0("shinytest2::test_app(\"", app_path_val, "\", filter = \"", test_filter, "\")"),
    #   "Or",
    #   " " = paste0("shinytest2::test_app(\"", app_path_val, "\")")
    #   # ,
    #   # "Or run all tests generically with:",
    #   # " " = paste0("shiny::runTests(\"", app_path_val, "\")")
    # ))


#' Launch test event recorder for a Shiny app
#'
#' Once a recording is completed, it will create or append a new \pkg{shinytest2} test to the \pkg{testthat} `test_file`.
#'
#' @param app A [`AppDriver`] object, or path to a Shiny
#'   application.
#' @param ... Must be empty. Allows for parameter expansion.
#' @param name Name provided to [`AppDriver`]. This value should be unique between all tests within a test file. If it is not unique, different expect methods may overwrite each other.
#' @param seed A random seed to set before running the app. This seed will also
#'   be used in the test script.
#' @param load_timeout Maximum time to wait for the Shiny application to load, in
#'   milliseconds. If a value is provided, it will be saved in the test script.
#' @param shiny_args A list of options to pass to `runApp()`. If a value
#'   is provided, it will be saved in the test script.
#' @param test_file Base file name of the \pkg{testthat} test file.
#' @param open_test_file If `TRUE`, the test file will be opened in an editor
#'   via [`file.edit()`] before executing.
#' @param allow_no_input_binding This value controls if events without input
#'   bindings are recorded.
#'   * If `TRUE`, events without input bindings are recorded.
#'   * If `FALSE`, events without input bindings are not recorded.
#'   * If `NULL` (default), if an updated input does not have a corresponding input, a modal dialog will be shown asking if unbound input events should be recorded.
#'   See [`AppDriver`]`$set_inputs()` for more information.
#' @param run_test If `TRUE`, `test_file` will be executed after saving the recording.
#' @export
record_test <- function(
  app = ".",
  ...,
  name = NULL,
  seed = NULL,
  load_timeout = NULL,
  shiny_args = list(),
  test_file = "test-shinytest2.R",
  open_test_file = rlang::is_interactive(),
  allow_no_input_binding = NULL,
  run_test = TRUE
) {
  ellipsis::check_dots_empty()

  for (class_val in c("shiny.appobj", "ShinyDriver")) {
    if (inherits(app, class_val)) {
      abort(paste0("Recording tests for ", class_val, " objects is not supported."))
    }
  }

  if (is.character(app)) {
    app_path_val <- app
    if (grepl("^http(s?)://", app)) {
      abort("Recording tests for remote apps is not supported.")
    }

    # Rmds need a random seed
    if (app_path(app, "app")$is_rmd && is.null(seed)) {
      seed <- floor(stats::runif(1, min = 0, max = 1e5))
    }

    app <- AppDriver$new(app, seed = seed, load_timeout = load_timeout, shiny_args = shiny_args)
    on.exit({
      rm(app)
      gc()
    })
  } else {
    app_path_val <- app$get_path()
  }

  if (!inherits(app, "AppDriver")) {
    abort("Unknown object type to record tests for. Must supply a `AppDriver` object or file path")
  }

  if (is.null(name)) {
    name <- fs::path_abs(app$get_path())
    if (!fs::dir_exists(name)) name <- fs::path_dir(name)
    name <- fs::path_file(name)
  }

  # Get the URL for the app. Depending on what type of object `app` is, it may
  # require starting an app.
  url <- app$get_url()

  # Are we running in RStudio? If so, we might need to fix up the URL so that
  # it's externally accessible.
  if (rstudio_is_available()) {
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
      shinytest2.recorder.url = url,
      shinytest2.app          = app,
      shinytest2.name         = name,
      shinytest2.load.timeout = if (!is.null(load_timeout)) load_timeout,
      shinytest2.seed         = seed,
      shinytest2.shiny.args   = shiny_args,
      shinytest2.test_file    = test_file,
      shinytest2.allow_no_input_binding = allow_no_input_binding
    ),
    # Make sure the recorder opens in an external browser
    with_external_shiny_browser({
      res <- shiny::runApp(system.file("internal", "recorder", package = "shinytest2"))
    })
  )

  saved_test_file <- res$test_file
  if (
    # Quit if the test file was not saved
    is.null(saved_test_file) ||
    # Quit if told not to run the test
    !isTRUE(run_test)
  ) {
    return(invisible(NULL))
  }

  if (isTRUE(open_test_file)) {
    edit_file(saved_test_file)
  }

  test_filter <- sub("^test-", "", fs::path_ext_remove(fs::path_file(saved_test_file)))
  # Run the test script
  rlang::inform("Running recorded test: ", fs::path_rel(saved_test_file, app_path_val))
  test_app(app_path_val, filter = test_filter)

  invisible(res$test_file)
}


#' Register an input processor for the test recorder
#'
#' @description
#' `register_input_processor()` registers an input processor which will be used by
#' the test recorder. The input processor function should take one parameter,
#' `value`, and return a string of R code which returns the desired value.
#'
#' `get_input_processors()` returns a named list of all registered input processor
#' functions.
#'
#' @param input_type The name of an input type, for example,
#'   `"mypkg.numberinput"`.
#' @param processor An input processor function.
#' @export
#' @keywords internal
register_input_processor <- function(input_type, processor) {
  if (!is.function(processor) || !identical(names(formals(processor)), "value")) {
    abort("`processor` must be a function that takes one parameter, `value`")
  }
  recorder_input_processors[[input_type]] <- processor
}

#' @rdname register_input_processor
#' @export
get_input_processors <- function() {
  as.list(recorder_input_processors)
}

# This environment holds input processors registered by other packages on load.
recorder_input_processors <- new.env(parent = emptyenv())