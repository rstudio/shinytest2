
# Try to warm up chromote. IDK why it fails on older versions of R.
test_that("Chromote loads", {
  skip_if(!on_ci(), "Not on CI")

  chromote_session <- chromote::default_chromote_object()$new_session()

  expect_s3_class(chromote_session, "ChromoteSession")
})
