# Import something from testthat to avoid a check error that nothing is imported from a `Depends` package
#' @importFrom testthat default_reporter
NULL

#' Test shiny application with testthat
#'
#' To be used within the apps `./tests` folder
#'
#' @param app_dir The base directory for the Shiny application
#' @param env Use the shiny application's environment after sourcing the R folder
#' @param reporter Uses the [`testthat::default_reporter()`] reporter but also includes a `fail` reporter to assert there are no errors found while testing
#' @param ... Parameters passed to [`testthat::test_dir()`]
#' @export
# Inspiration from https://github.com/rstudio/shiny/blob/a8c14dab9623c984a66fcd4824d8d448afb151e7/inst/app_template/tests/testthat.R
# TODO-barret; Name ok? `test_app()` Seems too close to "test this app" vs "calling testthat on app"
testthat_app <- function(
  app_dir = "../",
  # Run in the app's environment containing all support methods.
  env = shiny::loadSupport(app_dir),
  reporter = testthat::default_reporter(),
  ...
) {
  reporter <- reporter %||% "Progress"
  stopifnot(is.character(reporter))
  # force variables before testing starts / paths change
  list2(app_dir, env, reporter, ...)

  # By using this envvar, the DESCRIPTION file is not needed. Yay!
  # See `testthat::edition_get()` for usage
  withr::with_envvar(list(TESTTHAT_EDITION = 3), {
    testthat::test_dir(
      path = file.path(app_dir, "tests", "testthat"),
      env = env,
      # Display the regular progress output and throw an error if any test error is found
      # TODO-barret; Can this `Fail` logic be removed and check the output of the result instead?
      #   `testthat` does not throw an error if tests fail to execute
      reporter = c(reporter, "Fail"),
      ...
    )
  })
}
