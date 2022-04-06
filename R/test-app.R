# Import something from testthat to avoid a check error that nothing is imported from a `Depends` package
#' @importFrom testthat default_reporter
NULL

#' Test Shiny applications with \pkg{testthat}
#'
#' This is a helper method that wraps around [`testthat::test_dir()`] to test your Shiny application or Shiny runtime document.  This is similar to how [`testthat::test_check()`] tests your R package but for your app.
#'
#' When setting up tests for your app,
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
  ...
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

  is_currently_testing <- testthat::is_testing()

  ret <- testthat::test_dir(
    path = file.path(app_dir, "tests", "testthat"),
    ...
  )

  # If we are testing and no error has been thrown,
  # then perform an expectation so that the testing chunk passes
  if (is_currently_testing) {
    testthat::expect_equal(TRUE, TRUE)
  }

  invisible(ret)
}
