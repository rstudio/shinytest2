# Import something from testthat to avoid a check error that nothing is imported from a `Depends` package
#' @importFrom testthat default_reporter
NULL

#' Test shiny application with testthat
#'
#' To be used within the apps `./tests` folder
#'
#' @inheritParams testthat::test_dir
#' @param `./testthat` folder to execute tests over
#' @param env Use the shiny application's environment after sourcing the R folder
# TODO-barret; Can this be removed and check the output of the result instead?
#' @param reporter Use the default `testthat` reporter, but also include a `fail` reporter to assert there are no errors found
#' @export
# TODO-barret; Name ok? `test_app()` Seems too close to "test this app" vs "calling testthat on app"
# Inspiration from https://github.com/rstudio/shiny/blob/a8c14dab9623c984a66fcd4824d8d448afb151e7/inst/app_template/tests/testthat.R
testthat_app <- function(
  app_dir = "../",
  # Run in the app's environment containing all support methods.
  env = shiny::loadSupport(app_dir),
  reporter = default_reporter(),
  ...
) {
  # force variables before testing starts / paths change
  list2(app_dir, env, reporter, ...)

  # By using this envvar, the DESCRIPTION file is not needed. Yay!
  # See `testthat::edition_get()` for usage
  withr::with_envvar(list(TESTTHAT_EDITION = 3), {
    testthat::test_dir(
      path = file.path(app_dir, "tests", "testthat"),
      env = env,
      # Display the regular progress output and throw an error if any test error is found
      reporter = c(reporter, "Fail"),
      ...
    )
  })
}
