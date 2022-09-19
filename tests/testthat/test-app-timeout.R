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

expect_timeouts <- function(
  expected_load_timeout = 15 * 1000,
  expected_timeout = 4 * 1000,
  load_timeout = 15 * 1000,
  timeout = 4 * 1000,
  load_timeout_env = NULL,
  timeout_env = NULL,
  load_timeout_option = NULL,
  timeout_option = NULL
) {
  withr::with_envvar(
    list(
      "SHINYTEST2_LOAD_TIMEOUT" = load_timeout_env,
      "SHINYTEST2_TIMEOUT" = timeout_env
    ),
    withr::with_options(
      list(
        shinytest2.load_timeout = load_timeout_option,
        shinytest2.timeout = timeout_option
      ),
      {
        # Make sure values can be set
        testthat::expect_silent(
          AppDriver$new(app_url, load_timeout = load_timeout, timeout = timeout)
        )
        # Test values without R6 hackery
        private_env <- new.env(parent = emptyenv())
        app_init_timeouts(app, private_env, load_timeout = load_timeout, timeout = timeout)
        testthat::expect_equal(private_env$load_timeout, expected_load_timeout)
        testthat::expect_equal(private_env$timeout, expected_timeout)
      }
    )
  )

  # testthat::expect_equal(private_env$load_timeout, expected_load_timeout)
  # testthat::expect_equal(private_env$timeout, expected_timeout)
}

test_that("timeout initialization values", {

  # Respect given value
  expect_timeouts(
    expected_load_timeout = 1 * 1001,
    load_timeout          = 1 * 1001,
    load_timeout_env      = 2 * 1001,
    load_timeout_option   = 3 * 1001,

    expected_timeout      = 1 * 1002,
    timeout               = 1 * 1002,
    timeout_env           = 2 * 1002,
    timeout_option        = 3 * 1002
  )

  # Respect option value
  expect_timeouts(
    expected_load_timeout = 3 * 1001,
    load_timeout          = missing_arg(),
    load_timeout_env      = 2 * 1001,
    load_timeout_option   = 3 * 1001,

    expected_timeout      = 3 * 1002,
    timeout               = missing_arg(),
    timeout_env           = 2 * 1002,
    timeout_option        = 3 * 1002
  )

  # Respect env value
  expect_timeouts(
    expected_load_timeout = 2 * 1001,
    load_timeout          = missing_arg(),
    load_timeout_env      = 2 * 1001,
    load_timeout_option   = NULL,

    expected_timeout      = 2 * 1002,
    timeout               = missing_arg(),
    timeout_env           = 2 * 1002,
    timeout_option        = NULL
  )

  # Default values
  expect_timeouts(
    expected_load_timeout = 15 * 1000,
    load_timeout          = missing_arg(),
    load_timeout_env      = NULL,
    load_timeout_option   = NULL,

    expected_timeout      = 4 * 1000,
    timeout               = missing_arg(),
    timeout_env           = NULL,
    timeout_option        = NULL
  )
  # `NULL` Default values
  expect_timeouts(
    expected_load_timeout = 15 * 1000,
    load_timeout          = NULL,
    load_timeout_env      = NULL,
    load_timeout_option   = NULL,

    expected_timeout      = 4 * 1000,
    timeout               = NULL,
    timeout_env           = NULL,
    timeout_option        = NULL
  )
})
