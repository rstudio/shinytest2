test_that("Apps with duplicate output widget ids do not load currently", {
  expect_error(
    AppDriver$new(test_path("../../."), load_timeout = 2000),
    "Shiny app did not become stable"
  )
})
