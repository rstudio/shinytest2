# Import something from testthat to avoid a check error that nothing is imported from a `Depends` package
#' @importFrom testthat default_reporter
NULL

#' Test shiny application with testthat
#'
#' To be used within `./tests/testthat.R`.
#'
#' Example usage:
#' ```r
#' # Interactive usage
#' path_to_app <- "."
#' shinytest2::test_app(path_to_app)
#'
#' # ./tests/testthat.R
#' shinytest2::test_app()
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

  path_info <- app_path(app_dir, "app_dir")
  if (path_info$is_rmd) {
    app_dir <- path_info$app
  }

  # By using this envvar, the DESCRIPTION file is not needed. Yay!
  # See `testthat::edition_get()` for usage
  withr::with_envvar(list(TESTTHAT_EDITION = 3), {
    testthat::test_dir(
      path = file.path(app_dir, "tests", "testthat"),
      env = env,
      ...
    )
  })
}
