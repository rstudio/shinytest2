library(shinytest2)

test_that("Local pkg loads", {
  # No library calls!
  # expkg should be auto loaded by testthat

  expect_equal(expkg_internal_value, "expkg internal value")
  expect_equal(expkg_exported_value, "expkg exported value")
})


test_that("App requires library calls to load pkg", {
  app <- AppDriver$new("app.R")
  # app <- AppDriver$new()
  app$wait_for_idle()

  expect_equal(
    app$get_value(output = "btn_click_count"),
    "0"
  )

  app$click("btn")

  expect_equal(
    app$get_value(output = "btn_click_count"),
    "1"
  )
})
