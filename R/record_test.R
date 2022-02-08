#' Launch test event recorder for a Shiny app
#'
#' @param app A [AppDriver] object, or path to a Shiny
#'   application.
#' @param ... Must be empty. Allows for parameter expansion.
#' @param seed A random seed to set before running the app. This seed will also
#'   be used in the test script.
#' @param load_timeout Maximum time to wait for the Shiny application to load, in
#'   milliseconds. If a value is provided, it will be saved in the test script.
#' @param shiny_args A list of options to pass to `runApp()`. If a value
#'   is provided, it will be saved in the test script.
#' @export
# TODO-future; can we force the user to view the app in Chrome?
record_test <- function(
  app = ".",
  ...,
  seed = NULL,
  load_timeout = NULL,
  shiny_args = list(),
  test_file = "test-shinytest2.R",
  edit_test = rlang::is_interactive() && rstudioapi::isAvailable(),
  allow_input_no_binding = FALSE, # current,
  allow_no_input_binding = FALSE, # TODO-barret; change to _this variable_ throughout package
  run_test = TRUE # TODO-barret; handle this
) {
  ellipsis::check_dots_empty()

  checkmate::assert_true(rlang::is_installed("rstudioapi"))

  for (class_val in c("shiny.appobj", "ShinyDriver")) {
    if (inherits(app, class_val)) {
      abort(paste0("Recording tests for ", class_val, " objects is not supported."))
    }
  }

  if (is.character(app)) {
    if (grepl("^http(s?)://", app)) {
      abort("Recording tests for remote apps is not yet supported.")
    }

    app_path_info <- app_path(app, "app")
    app_dir <- app_path_info$dir
    app_path <- app_path_info$app
    is_rmd <- app_path_info$is_rmd

    # Rmds need a random seed
    if (is_rmd && is.null(seed)) {
      seed <- floor(stats::runif(1, min = 0, max = 1e5))
    }

    app <- AppDriver$new(app_path, seed = seed, load_timeout = load_timeout, shiny_args = shiny_args)
    on.exit({
      rm(app)
      gc()
    })
  }

  if (!inherits(app, "AppDriver")) {
    abort("Unknown object type to record tests for. Must supply a `AppDriver` object or file path")
  }

  # Get the URL for the app. Depending on what type of object `app` is, it may
  # require starting an app.
  url <- app$get_url()

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
      shinytest2.recorder.url = url,
      shinytest2.app          = app,
      shinytest2.load.timeout = if (!is.null(load_timeout)) load_timeout,
      shinytest2.seed         = seed,
      shinytest2.shiny.args   = shiny_args,
      shinytest2.test_file    = test_file,
      shinytest2.allow_input_no_binding = allow_input_no_binding
    ),
    res <- shiny::runApp(system.file("internal", "recorder", package = "shinytest2"))
  )


  if (rlang::has_name(res, "run")) {

    if (isTRUE(edit_test)) {
      file.edit(save_file)
    }

    test_filter <- sub("^test-", "", fs::path_ext_remove(test_file))
    if (isTRUE(res$run)) {
      # Run the test script
      test_app(app_path, filter = test_filter)
    } else {
      if (length(res$dont_run_reasons) > 0) {
        rlang::inform(c("Not running test script because", res$dont_run_reasons))
      }

      rlang::inform(paste0(
        "After making changes to the test script, run it with:\n",
        "  shinytest::test_app(\"", app_path, "\", filter = \"", test_filter, "\")\n",
        "Or run all tests generically with:\n",
        "  shiny::runTests(\"", app_path, "\")\n",
        "Or run all {testthat} tests using edition 3:\n",
        "  shinytest::test_app(\"", app_path, "\")"
      ))
    }
  }

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
