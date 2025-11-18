library(shinytest2)

test_that("Local pkg loads", {
  # No library calls!
  # expkg should be auto loaded by testthat

  expect_equal(expkg_internal_value, "expkg internal value")
  expect_equal(expkg_exported_value, "expkg exported value")
})


test_that("App requires library calls to load pkg", {
  app <- AppDriver$new("apps/library/")
  expkg_expect_shiny_app(app)
})

test_that("App requires require calls to load pkg", {
  # skip("because")
  app <- AppDriver$new("apps/require/")
  expkg_expect_shiny_app(app)
})
