library(shinytest2)

test_that("Local pkg loads", {
  # No library calls!
  # expkg should be auto loaded by testthat

  expect_(expkg_shiny_app, "expkg internal value")
  expect_equal(expkg_exported_value, "expkg exported value")
})

test_that("App function requires library calls to load pkg", {
  app <- AppDriver$new(function() {
    shiny::runApp(expkg_shiny_app())
  })
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

test_that("App function requires library calls to load pkg", {
  app <- AppDriver$new(expkg_shiny_app)
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
