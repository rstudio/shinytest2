
# This test content should test all testthat functions or values used
# within `./R/test-app.R` to make sure their API does not change.
# This should always run on CRAN to prevent breakages!!
custom_test_name <- "testthat does not change their API"
test_that(custom_test_name, {
  reporter <- testthat::get_reporter()

  # Normalize reporter value
  outer_reporter <- testthat::with_reporter(
    reporter,
    testthat::get_reporter(),
    start_end_reporter = FALSE
  )

  str(outer_reporter)

  # Make sure it is a multireporter
  expect_s3_class(outer_reporter, "MultiReporter")
  expect_true(".context" %in% names(outer_reporter))

  outer_context <- outer_reporter$.context
  expect_equal(outer_context, "test-app-reporter")

  # Find the SnapshotReporter, as the `test` value is available
  snapshot_reporters <- Filter(outer_reporter$reporters, f = function(x) inherits(x, "SnapshotReporter"))
  expect_true(length(snapshot_reporters) > 0)

  snapshot_reporter <- snapshot_reporters[[1]]
  expect_true("test" %in% names(snapshot_reporter))
  expect_equal(snapshot_reporter$test, custom_test_name)

  # Test all replay reporter methods used and number of args used
  for (api_def in list(
    list(name = "is_full"),
    list(name = "start_context"),
    list(name = "end_context"),
    list(name = "end_context_if_started"),
    list(name = "add_result"),
    list(name = "start_file", args_min_len = 1),
    list(name = "end_file"),
    list(name = "start_test", args_min_len = 2),
    list(name = "end_test")
  )) {
    expect_true(api_def$name %in% names(outer_reporter))
    if (!is.null(api_def$args_min_len)) {
      outer_reporter_arg_count <- length(formals(outer_reporter[[api_def$name]]))
      expect_true(outer_reporter_arg_count >= api_def$args_min_len)
    }
  }
})
