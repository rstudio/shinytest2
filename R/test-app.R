# Import something from testthat to avoid a check error that nothing is imported from a `Depends` package
#' @importFrom testthat default_reporter
NULL

#' Test shiny application with testthat
#'
#' Example usage:
#' ```r
#' # Interactive usage
#' path_to_app <- "."
#' shinytest2::test_app(path_to_app)
#'
#' # File: ./tests/testthat.R
#' shinytest2::test_app()
#'
#' # File: ./tests/testthat/test-shinytest2.R
#' test_that("Testing an external app", {
#'   shinytest2::test_app(path_to_app)
#' })
#' ```
#'
#' @param app_dir The base directory for the Shiny application
#' @param env Use the shiny application's environment after sourcing the R folder
#' @param ... Parameters passed to [`testthat::test_dir()`]
#' @export
# Inspiration from https://github.com/rstudio/shiny/blob/a8c14dab9623c984a66fcd4824d8d448afb151e7/inst/app_template/tests/testthat.R
test_app <- function(
  app_dir = "../",
  # Run in the app's environment containing all support methods.
  env = shiny::loadSupport(app_dir),
  ...
) {
  library(shinytest2)
  # force variables before testing starts / paths change
  list2(app_dir, env, ...)

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
