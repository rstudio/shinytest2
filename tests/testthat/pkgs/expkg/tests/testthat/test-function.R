library(shinytest2)

test_that("Local pkg loads", {
  # No library calls!
  # expkg should be auto loaded by testthat

  expect_equal(expkg_exported_value, "expkg exported value")
  expect_equal(expkg_internal_value, "expkg internal value")
})


test_that("Pass in fn directly", {
  app <- AppDriver$new(expkg_run_shiny_app)
  expkg_expect_shiny_app(app)
})
