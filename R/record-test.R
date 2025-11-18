warning(
  "TODO-barret; Record test should save the recording into the package if within a package."
)

#' Launch test event recorder for a Shiny app
#'
#' Once a recording is completed, it will create or append a new
#' \pkg{shinytest2} test to the \pkg{testthat} `test_file`.
#'
#' @section Uploading files:
#'
#' Files that are uploaded to your Shiny app must be located somewhere within
#' the `tests/testthat` subdirectory or available via `system.file()`.
#'
#' Files that are uploaded during recording that do not have a valid path will
#' have a warning inserted into the code. Please fix the file path by moving the
#' file to the `tests/testthat` subdirectory or by using `system.file()`. After
#' fixing the path, remove the line of warning code.
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
#'   * If `NULL` (default), if an updated input does not have a corresponding
#'     input, a modal dialog will be shown asking if unbound input events should
#'     be recorded.
#'
#'   See [`AppDriver`]`$set_inputs()` for more information.
#' @param record_screen_size If `TRUE`, the screen size will be recorded when initialized and changed.
#' @param run_test If `TRUE`, `test_file` will be executed after saving the recording.
#' @seealso [`test_app()`]
#' @export
#' @examples
#' \dontrun{
#' record_test("path/to/app")
#' }
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
  record_screen_size = TRUE,
  run_test = TRUE
) {
  rlang::check_dots_empty()
  rlang::check_installed("shinyvalidate", "0.1.2")

  if (inherits(app, "ShinyDriver")) {
    rlang::abort(paste0(
      "Recording tests for `ShinyDriver` objects is not supported."
    ))
  }
  if (shiny::is.shiny.appobj(app)) {
    app <- AppDriver$new(app)
  }

  if (is.character(app)) {
    app_path_val <- app
    if (grepl("^http(s?)://", app)) {
      rlang::abort("Recording tests for remote apps is not supported.")
    }

    # Rmds need a random seed
    if (is.null(seed) && app_dir_has_rmd(app_dir = app)) {
      seed <- floor(stats::runif(1, min = 0, max = 1e5))
    }

    app <- AppDriver$new(
      app,
      seed = seed,
      load_timeout = load_timeout,
      shiny_args = shiny_args
    )
    on.exit({
      rm(app)
      gc()
    })
  } else {
    app_path_val <- app$get_dir()
  }

  if (!inherits(app, "AppDriver")) {
    rlang::abort(
      "Unknown object type to record tests for. Must supply a `AppDriver` object or file path"
    )
  }

  if (is.null(name)) {
    name <- fs::path_file(app$get_dir())
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
      shinytest2.app = app,
      shinytest2.name = name,
      shinytest2.load.timeout = if (!is.null(load_timeout)) load_timeout,
      shinytest2.seed = seed,
      shinytest2.shiny.args = shiny_args,
      shinytest2.test_file = test_file,
      shinytest2.record_screen_size = isTRUE(record_screen_size),
      shinytest2.allow_no_input_binding = allow_no_input_binding
    ),
    # Make sure the recorder opens in an external browser
    with_external_shiny_browser({
      res <- shiny::runApp(system.file(
        "internal",
        "recorder",
        package = "shinytest2"
      ))
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

  edit_file(saved_test_file, open = open_test_file)

  test_filter <- sub(
    "^test-",
    "",
    fs::path_ext_remove(fs::path_file(saved_test_file))
  )
  # Run the test script
  rlang::inform(c(
    "*" = paste0(
      "Running recorded test: ",
      fs::path_rel(saved_test_file, app$get_dir())
    )
  ))
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
  if (
    !is.function(processor) || !identical(names(formals(processor)), "value")
  ) {
    rlang::abort(
      "`processor` must be a function that takes one parameter, `value`"
    )
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
