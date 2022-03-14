# Import something from testthat to avoid a check error that nothing is imported from a `Depends` package
#' @importFrom testthat default_reporter
NULL

#' Test shiny application with testthat
#'
#' Example usage:
#' ```r
#' ## Interactive usage
#' # Test Shiny app in current working directory
#' shinytest2::test_app()
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
#' test_that("Testing an external app", {
#'   shinytest2::test_app(path_to_app)
#' })
#' ```
#'
#' @param app_dir The base directory for the Shiny application.
#'   * If `app_dir` is missing and `test_app()` is called within the
#'     `./tests/testthat.R` file, the parent directory (`"../"`) is used.
#'   * Otherwise, the default path of `"."` is used.
#' @param env Use the shiny application's environment after sourcing the R folder
#' @param ... Parameters passed to [`testthat::test_dir()`]
#' @export
# Inspiration from https://github.com/rstudio/shiny/blob/a8c14dab9623c984a66fcd4824d8d448afb151e7/inst/app_template/tests/testthat.R
test_app <- function(
  app_dir = missing_arg(),
  # Run in the app's environment containing all support methods.
  env = shiny::loadSupport(app_dir),
  ...
) {
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

  # By using this envvar, the DESCRIPTION file is not needed. Yay!
  # See `testthat::edition_get()` for usage
  withr::with_envvar(list(TESTTHAT_EDITION = 3), {
    ret <- testthat::test_dir(
      path = file.path(app_dir, "tests", "testthat"),
      env = env,
      ...
    )
  })

  # If we are testing and no error has been thrown,
  # then perform an expectation so that the testing chunk passes
  if (is_currently_testing) {
    testthat::expect_equal(TRUE, TRUE)
  }

  invisible(ret)
}
