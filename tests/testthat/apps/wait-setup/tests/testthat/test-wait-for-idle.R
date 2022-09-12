
# Testing that `./setup.R` is loaded

test_that("wait for idle works", {

  app <- AppDriver$new()
  withr::defer(app$stop())

  app$wait_for_idle(duration = 2 * n)

  expect_equal(app$get_value(output = "txt"), "1 2 3")
})
