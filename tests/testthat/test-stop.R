test_that("Stopping the app listens to the signals", {
  expect_signal_timeout <- function(
    expected_signal_timeout,
    signal_timeout = missing_arg(),
    signal_timeout_env = NULL,
    signal_timeout_option = NULL,
    covr_is_set = FALSE
  ) {
    withr::with_envvar(
      list(
        "SHINYTEST2_SIGNAL_TIMEOUT" = signal_timeout_env,
        "R_COVR" = if (covr_is_set) "true" else NULL
      ),
      withr::with_options(
        list(
          shinytest2.signal_timeout = signal_timeout_option
        ),
        {
          # Use init method to avoid timing issues while testing
          signal_timeout_val <- resolve_signal_timeout(signal_timeout)
          testthat::expect_equal(signal_timeout_val, expected_signal_timeout)
        }
      )
    )
  }

  # Respect given value
  expect_signal_timeout(
    expected_signal_timeout = 1 * 1001,
    signal_timeout          = 1 * 1001,
    signal_timeout_env      = 2 * 1001,
    signal_timeout_option   = 3 * 1001
  )
  # Respect option value
  expect_signal_timeout(
    expected_signal_timeout = 3 * 1001,
    signal_timeout          = missing_arg(),
    signal_timeout_env      = 2 * 1001,
    signal_timeout_option   = 3 * 1001
  )
  # Respect env value
  expect_signal_timeout(
    expected_signal_timeout = 2 * 1001,
    signal_timeout          = missing_arg(),
    signal_timeout_env      = 2 * 1001,
    signal_timeout_option   = NULL
  )
  # Default values
  expect_signal_timeout(
    expected_signal_timeout = 500,
    signal_timeout          = missing_arg(),
    signal_timeout_env      = NULL,
    signal_timeout_option   = NULL
  )
  expect_signal_timeout(
    expected_signal_timeout = 20 * 1000,
    signal_timeout          = missing_arg(),
    signal_timeout_env      = NULL,
    signal_timeout_option   = NULL,
    covr_is_set             = TRUE
  )

})
