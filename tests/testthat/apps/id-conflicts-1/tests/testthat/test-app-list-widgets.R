test_that("warn for multiple widgets sharing an ID", {
  expect_warning(
    ShinyDriver2$new(test_path("../../.")),
    "Possible duplicate input widget ids: select"
  )
})
