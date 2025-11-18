warning("TODO-barret; test check_setup values")
warning(
  "TODO-barret; requires R file loading; Look into using `testthat::test_dir(load_package=)` and `testthat:::test_files_setup_env()`"
)

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
#'   app <- shinytest2::AppDriver$new(path_to_app)
#'   # Perform tests with `app`...
#' })
#' ```
#'
#' When testing within a package, it is recommended to not call `test_app()`,
#' but instead test your applications within your own \pkg{testthat} tests. This
#' allows for more flexibility and control over how your applications are
#' tested while your current package's testthat infrastructure. See the [Use
#' Package
#' vignette](https://rstudio.github.io/shinytest2/articles/use-package.html) for
#' more details.
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
#' @param reporter Reporter to pass through to [`testthat::test_dir()`].
#' @param stop_on_failure If missing, the default value of `TRUE` will be used. However, if missing and currently testing, `FALSE` will be used to seamlessly integrate the app reporter to `reporter`.
#' @param check_setup [Deprecated]. Parameter ignored.
#' @seealso
#' * [`record_test()`] to create tests to record against your Shiny application.
#' * [testthat::snapshot_review()] and [testthat::snapshot_accept()] if
#'   you want to compare or update snapshots after testing.
#' * [`load_app_env()`] to load the Shiny application's helper files.
#'   This is only necessary if you want access to the values while testing.
#'
#' @export
#' @importFrom lifecycle deprecated
test_app <- function(
  app_dir = missing_arg(),
  ...,
  name = missing_arg(),
  reporter = testthat::get_reporter(),
  stop_on_failure = missing_arg(),
  check_setup = deprecated()
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

  # If value is provided and `TRUE`...
  if (lifecycle::is_present(check_setup)) {
    if (isTRUE(check_setup)) {
      lifecycle::deprecate_warn(
        "0.5.0",
        "shinytest2::test_app(check_setup = 'is no longer used')",
        details = c(
          "To manually load an app's support files, call `shinytest2::local_app_support(app_dir=)` within your {testthat} test",
          i = "Please see `?shinytest2::local_app_support` for more information"
        )
      )
    }
  }

  if (testthat::is_testing()) {
    rlang::warn(
      c(
        "x" = "Calling {.code shinytest2::test_app() } within a {.pkg testthat } test has been superseded in {.pkg shinytest2 } v0.5.0",
        "i" = "If you are testing within a package, please see URL on how to migrate your App tests to be located in your package tests.",
        "i" = "If you are using CI, don't forget to collect your new snapshots after your initial run!",
        "i" = "See {.url https://rstudio.github.io/shinytest2/articles/use-package.html } for more details."
      ),
      use_cli_format = TRUE,
      .frequency = "once",
      .frequency_id = "shinytest2_test_app_migration_warning"
    )
    # warning("TODO-barret; missing url for migration warning")

    # Normalize the reporter given any input
    outer_reporter <- testthat::with_reporter(
      reporter,
      testthat::get_reporter(),
      start_end_reporter = FALSE
    )

    outer_context <- NULL

    # If a test is currently active, stop it and restart on exit
    test_name <- rlang::missing_arg()
    snapshot_reporter <- NULL
    if (inherits(outer_reporter, "SnapshotReporter")) {
      snapshot_reporter <- outer_reporter
    } else if (inherits(outer_reporter, "MultiReporter")) {
      # Find the SnapshotReporter, as the `test` value is available
      snapshot_reporters <- Filter(outer_reporter$reporters, f = function(x) {
        inherits(x, "SnapshotReporter")
      })
      if (length(snapshot_reporters) > 0) {
        snapshot_reporter <- snapshot_reporters[[1]]
      }
    }
    if (!is.null(snapshot_reporter)) {
      test_name <- snapshot_reporter$test
      outer_context <- snapshot_reporter$file
      if (!is.null(test_name)) {
        # End the current test
        snapshot_reporter$end_test(outer_context, test_name)
      }

      ## Unwravel and re-wrap file/context like
      ## https://github.com/r-lib/testthat/blob/aab0464b555c27dcb2381af5f71c395a084a8643/R/test-files.R#L269-L277
      # Stop the current context / file
      outer_reporter$end_context_if_started()
      outer_reporter$end_file()
      withr::defer({
        # Restore the context when done
        outer_reporter$start_file(outer_context)

        if (!rlang::is_missing(test_name)) {
          # Restore the current test
          outer_reporter$start_test(outer_context, test_name)
        }
      })

      name <- rlang::maybe_missing(name, fs::path_file(app_dir))

      # nolint start
      ReplayReporter <- R6Class(
        # nolint end
        "ReplayReporter",
        inherit = testthat::Reporter,
        public = list(
          is_full = function(...) outer_reporter$is_full(...),
          ## Do not perform these two methods.
          ## We want to act like a continuously integrated reporter
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
            # outer_reporter$end_file()

            # Upgrade the name
            if (
              !is.null(name) &&
                length(name) == 1 &&
                is.character(name) &&
                nchar(name) > 0
            ) {
              # ⠏ |         0 | CURRENT_TEST_CONTEXT - APP_NAME - APP_TEST_FILE
              test_file <- sub(
                "^test-",
                paste0(
                  "test-",
                  if (is.null(outer_context)) {
                    ""
                  } else {
                    paste0(outer_context, " - ")
                  },
                  name,
                  " - "
                ),
                test_file
              )
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
      # Currently testing, the inner reporter should not stop on failure
      stop_on_failure <- rlang::maybe_missing(stop_on_failure, FALSE)
    }
  } else {
    # Not currently testing
    # Use the default stop_on_failure value
    stop_on_failure <-
      rlang::maybe_missing(
        stop_on_failure,
        formals(testthat::test_dir)$stop_on_failure
      )
  }

  results <- testthat::test_dir(
    path = fs::path(app_dir, "tests", "testthat"),
    reporter = reporter,
    stop_on_failure = stop_on_failure,
    ...
  )

  invisible(results)
}


#' Load the Shiny application's support environment
#'
#' `r lifecycle::badge("superseded")` by `local_app_support()` and
#' `with_app_support()` as they offer more flexibility where and when the
#' support environment is loaded.
#'
#' Executes all `./R` files and `global.R` into the current environment. This is
#' useful when wanting access to functions or values created in the `./R` folder
#' for testing purposes.
#'
#' Loading these files is not automatically performed by `test_app()` and should
#' be called in `./tests/testthat/setup-shinytest2.R` if access to support file
#' objects is desired.
#'
#' @seealso [`use_shinytest2()`] for creating a testing setup file that
#'   loads your Shiny app support environment into the testing environment.
#'
#' @param app_dir The base directory for the Shiny application.
#' @param renv The environment in which the files in the `R/`` directory should
#' be evaluated.
#' @inheritParams shiny::loadSupport
#' @export
load_app_env <- function(
  app_dir = "../../",
  renv = rlang::caller_env(),
  globalrenv = rlang::caller_env()
) {
  shiny::loadSupport(app_dir, renv = renv, globalrenv = globalrenv)
}

#' Attach the Shiny application's support environment
#'
#' Executes all `./R` files and `global.R` into a temp environment that is
#' attached appropriately. This is useful when wanting access to functions or
#' values created in the `./R` folder for testing purposes.
#'
#' Loading these files is not automatically performed by `test_app()` and must
#' be called within your unit tests to access support file objects.
#'
#' @param app_dir The base directory for the Shiny application.
#' @param expr An expression to evaluate within the support environment.
#' @param envir The environment in which the files in the `R/` directory should
#' be evaluated.
#' @describeIn app_support Temporarily attach the Shiny application's support
#' environment into the current environment.
#' @export
#' @examples
#' \dontrun{
#' # ./R/utils.R
#' n <- 42
#'
#' #' # ./tests/testthat/test-utils.R
#' test_that("Can access support environment", {
#'   shinytest2::local_app_support()
#'   expect_equal(n, 42)
#' })
#' }
local_app_support <- function(
  app_dir = "../../",
  envir = rlang::caller_env()
) {
  globalrenv <- rlang::new_environment(parent = envir)
  renv <- rlang::new_environment(parent = globalrenv)

  shiny::loadSupport(app_dir, renv = renv, globalrenv = globalrenv)

  withr::local_environment(renv, .local_envir = envir)
}

#' @describeIn app_support For the provided `expr`, attach the Shiny
#' application's support environment into the current environment.
#' @export
with_app_support <- function(
  expr,
  app_dir = "../../",
  envir = rlang::caller_env()
) {
  globalrenv <- rlang::new_environment(parent = envir)
  renv <- rlang::new_environment(parent = globalrenv)

  shiny::loadSupport(app_dir, renv = renv, globalrenv = globalrenv)

  withr::with_environment(renv, expr)
}
