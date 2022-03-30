
# Try to warm up chromote. IDK why it fails on older versions of R.
test_that("Chromote loads", {
  skip_if(!on_ci(), "Not on CI")

  # Wrap in a `try()` as the test doesn't matter
  # Only the action of trying to open chromote matters
  try({
    chromote::default_chromote_object()$new_session()
  })

  expect_true(TRUE)
})
