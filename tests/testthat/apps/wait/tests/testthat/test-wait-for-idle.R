# Testing that `./setup-shinytest2.R` is loaded

test_that("wait for idle works", {

  app <- AppDriver$new()
  withr::defer(app$stop())

  app$wait_for_idle(duration = 2 * n)

  expect_equal(app$get_value(output = "txt"), "1 2 3")
})


test_that("waiting a lesser value will not be enough", {

  app <- AppDriver$new()
  withr::defer(app$stop())

  app$wait_for_idle(duration = n / 2)

  expect_failure(
    expect_equal(app$get_value(output = "txt"), "1 2 3")
  )
})
