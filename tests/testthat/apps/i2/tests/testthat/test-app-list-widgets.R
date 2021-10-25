test_that("warn for multiple widgets sharing an ID", {
  ## Actually apps, with duplicate output widget ids do not load currently
  expect_error(
    ShinyDriver2$new(test_path("../../."), load_timeout = 2000),
    "Shiny app did not load"
  )
})
