test_that("Apps with duplicate output widget ids do not load currently", {
  expect_warning(
    AppDriver$new(test_path("../../.")),
    "Widget ids both for input and output: widget"
  )
})
