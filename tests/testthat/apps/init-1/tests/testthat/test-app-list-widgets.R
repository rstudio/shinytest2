test_that("warn for multiple widgets sharing an ID", {
  expect_warning(
    AppDriver$new(test_path("../../.")),
    "Possible duplicate input widget ids: select"
  )
})
