require("shiny", quietly = TRUE, character.only = TRUE)

shiny_app <- shinyApp(
  ui = fluidPage(
    # empty
  ),
  server = function(input, output) {
    # empty
  }
)
# Start single app in background
app <- AppDriver$new(shiny_app)
withr::defer(app$stop())
# Get app url to avoid startup time for each test
app_url <- app$get_url()

expect_timeout <- function(
  expected_stepsize = 1000,
  ci = c("false", "true"),
  option_stepsize = NULL,
  new_stepsize = rlang::missing_arg()
) {
  app <- withr::with_envvar(
    list("CI" = match.arg(ci)),
    withr::with_options(
      list(shinytest2.timeout.stepsize = option_stepsize),
      {
        AppDriver$new(app_url, timeout_stepsize = new_stepsize)
      }
    )
  )
  k <- 4
  testthat::expect_equal(app$get_timeout(k), k * expected_stepsize)

  # Make sure we can override the value after init
  n <- 3232
  app$set_timeout(n)
  testthat::expect_equal(app$get_timeout(k), k * n)
}

test_that("timeout step size initialization values", {
  # Default stepsize
  expect_timeout(1000, ci = "false", option_stepsize = NULL, new_stepsize = missing_arg())
  expect_timeout(2000, ci = "true", option_stepsize = NULL, new_stepsize = missing_arg())

  # Option set
  expect_timeout(1234, ci = "false", option_stepsize = 1234, new_stepsize = missing_arg())
  expect_timeout(1234, ci = "true", option_stepsize = 1234, new_stepsize = missing_arg())

  # Supplied value has preference
  expect_timeout(5678, ci = "false", option_stepsize = NULL, new_stepsize = 5678)
  expect_timeout(5678, ci = "true", option_stepsize = NULL, new_stepsize = 5678)
  expect_timeout(5678, ci = "false", option_stepsize = 1234, new_stepsize = 5678)
  expect_timeout(5678, ci = "true", option_stepsize = 1234, new_stepsize = 5678)
})
