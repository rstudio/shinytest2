# Import something from testthat to avoid a check error that nothing is imported from a `Depends` package
#' @importFrom testthat default_reporter
NULL

#' Test Shiny applications with \pkg{testthat}
#'
#' This is a helper method that wraps around [`testthat::test_dir()`] to test
#' your Shiny application or Shiny runtime document.  This is similar to how
#' [`testthat::test_check()`] tests your R package but for your app.
#'
#' @details
#'
#' Example usage:
#'
#' ```{r, eval = FALSE}
#' ## Interactive usage
#' # Test Shiny app in current working directory
#' shinytest2::test_app()
#'
#' # Test Shiny app in another directory
#' path_to_app <- "path/to/app"
#' shinytest2::test_app(path_to_app)
#'
#' ## File: ./tests/testthat.R
#' # Will find Shiny app in "../"
#' shinytest2::test_app()
#'
#' ## File: ./tests/testthat/test-shinytest2.R
#' # Test a shiny application within your own {testthat} code
#' test_that("Testing a Shiny app in a package", {
#'   shinytest2::test_app(path_to_app)
#' })
#' ```
#'
#' @section Uploading files:
#'
#' When testing an application, all non-temp files that are uploaded should be
#' located in the `./tests/testthat` directory. This allows for tests to be more
#' portable and self contained.
#'
#' When recording a test with [`record_test()`], for every uploaded file that is
#' located outside of `./tests/testthat`, a warning will be thrown. Once the
#' file path has be fixed, you may remove the warning statement.
#'
#' @section Different ways to test:
#'
#' `test_app()` is an opinionated testing function that will only execute
#' \pkg{testthat} tests in the `./tests/testthat` folder. If (for some rare
#' reason) you have other non-\pkg{testthat} tests to execute, you can call
#' [`shiny::runTests()`]. This method will generically run all test runners and
#' their associated tests.
#'
#' ```r
#' # Execute a single Shiny app's {testthat} file such as `./tests/testthat/test-shinytest2.R`
#' test_app(filter = "shinytest2")
#'
#' # Execute all {testthat} tests
#' test_app()
#'
#' # Execute all tests for all test runners
#' shiny::runTests()
#' ```
#'
#' @param app_dir The base directory for the Shiny application.
#'   * If `app_dir` is missing and `test_app()` is called within the
#'     `./tests/testthat.R` file, the parent directory (`"../"`) is used.
#'   * Otherwise, the default path of `"."` is used.
#' @param ... Parameters passed to [`testthat::test_dir()`]
#' @param reporter The reporter to use for the tests
#' @param name Name to display in the middle of the test name. This value is only used
#' when calling `test_app()` inside of \pkg{testhat} test. The final testing context will
#' have the format of `"{test_context} - {name} - {app_test_context}"`.
#' @param check_setup If `TRUE`, the app will be checked for the presence of
#' `./tests/testthat/setup.R`. This file must contain a call to
#' [`shinytest2::load_app_env()`].
#' @seealso
#' * [`record_test()`] to create tests to record against your Shiny application.
#' * [testthat::snapshot_review()] and [testthat::snapshot_accept()] if
#'   you want to compare or update snapshots after testing.
#' * [`load_app_env()`] to load the Shiny application's helper files.
#'   This is only necessary if you want access to the values while testing.
#'
#' @export
test_app <- function(
  app_dir = missing_arg(),
  ...,
  reporter = testthat::get_reporter(),
  name = missing_arg(),
  check_setup = TRUE
) {
  # Inspiration from https://github.com/rstudio/shiny/blob/a8c14dab9623c984a66fcd4824d8d448afb151e7/inst/app_template/tests/testthat.R

  library(shinytest2)

  app_dir <- rlang::maybe_missing(app_dir, {
    cur_path <- fs::path_abs(".")
    cur_folder <- fs::path_file(cur_path)
    sibling_folders <- fs::path_file(fs::dir_ls(cur_path))
    if (
      length(cur_folder) == 1 &&
      cur_folder == "tests" &&
      "testthat" %in% sibling_folders
    ) {
      "../"
    # } else if ("tests" %in% sibling_folders) {
    #   "."
    } else {
      "."
    }
  })

  app_dir <- app_dir_value(app_dir)

  if (isTRUE(check_setup)) {
    setup_path <- fs::path(app_dir, "tests", "testthat", "setup.R")
    if (!fs::file_exists(setup_path)) {
      rlang::abort(
        c(
          "No `setup.R` file found in `./tests/testthat`",
          "i" = paste0("To create a `setup.R` file, please run `shinytest2::use_shinytest2(\"", app_dir, "\", setup = TRUE)`"),
          "i" = "To disable this message, please set `test_app(check_setup = FALSE)`"
        )
      )
    }
    lines <- read_utf8(setup_path)
    if (!grepl("load_app_env", lines, fixed = TRUE)) {
      rlang::abort(
        c(
          "No call to `shinytest2::load_app_env()` found in `./tests/testthat/setup.R`",
          "i" = paste0("To create a `setup.R` file, please run `shinytest2::use_shinytest2(\"", app_dir, "\", setup = TRUE)`"),
          "i" = "To disable this message, please set `test_app(check_setup = FALSE)`"
        )
      )
    }
  }



  if (testthat::is_testing()) {
    # Normalize the reporter given any input
    outer_reporter <- testthat::with_reporter(reporter, testthat::get_reporter(), start_end_reporter = FALSE)

    outer_context <- outer_reporter$.context
    # Stop the current context
    outer_reporter$end_context(outer_context)
    withr::defer({
      # Restore the context when done
      outer_reporter$start_context(outer_context)
    })

    name <- rlang::maybe_missing(name, fs::path_file(app_dir))

    ReplayReporter <- R6Class( # nolint
      "ReplayReporter",
      inherit = testthat::Reporter,
      public = list(
        is_full = function(...) outer_reporter$is_full(...),
        ## Do not perform these two methods. We want to act like a continuously integrated reporter
        # start_reporter = outer_reporter$start_reporter,
        # end_reporter = outer_reporter$end_reporter,
        start_context = function(...) outer_reporter$start_context(...),
        end_context = function(...) outer_reporter$end_context(...),
        add_result = function(...) outer_reporter$add_result(...),
        start_file = function(test_file, ...) {
          ## This could be done above when ending the outer context
          ## However, by ending / starting the outer file
          ## a hint is displayed as to what file is currently testing
          # Close current file
          outer_reporter$end_file()

          # Upgrade the name
          if (!is.null(name) && length(name) == 1 && is.character(name) && nchar(name) > 0) {
            # ⠏ |         0 | CURRENT_TEST_CONTEXT - APP_NAME - APP_TEST_FILE
            test_file <- sub("^test-", paste0("test-", outer_context, " - ", name, " - "), test_file)
          }
          outer_reporter$start_file(test_file, ...)
        },
        end_file = function(...) {
          outer_reporter$end_file(...)
          # Restart current file that was ended in ReplayReporter$start_file
          outer_reporter$start_file(outer_context)
        },
        start_test = function(...) outer_reporter$start_test(...),
        end_test = function(...) outer_reporter$end_test(...)
      )
    )
    reporter <- ReplayReporter$new()
  }

  results <- testthat::test_dir(
    path = fs::path(app_dir, "tests", "testthat"),
    reporter = reporter,
    ...
  )

  invisible(results)
}



#' Load the Shiny application's support environment
#'
#' Executes all `./R` files and `global.R` into the current environment.
#' This is useful when wanting access to functions or values created in the `./R` folder for testing purposes.
#'
#' Loading these files is not automatically performed by `test_app()` and must
#' be called in `./tests/testthat/setup.R` if access to support file objects is
#' desired.
#'
#' @seealso [`use_shinytest2()`] for creating a testing setup file that
#'   loads your Shiny app support environment into the testing environment.
#'
#' @param app_dir The base directory for the Shiny application.
#' @param renv The environment in which the files in the `R/`` directory should be evaluated.
#' @inheritParams shiny::loadSupport
#' @export
load_app_env <- function(
  app_dir = "../../",
  renv = rlang::caller_env(),
  globalrenv = rlang::caller_env()
) {
  shiny::loadSupport(app_dir, renv = renv, globalrenv = globalrenv)
}
