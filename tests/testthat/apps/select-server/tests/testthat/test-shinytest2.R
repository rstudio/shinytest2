library(shinytest2)

test_that("{shinytest2} recording: set-six", {

  app <- AppDriver$new(name = "test")
  withr::defer(app$stop())

  # works, prints "5"
  app$set_inputs(select = 5)
  expect_equal(app$get_value(input = "select"), "5")

  # doesn't work, prints ""
  app$set_inputs(select = 6)
  expect_equal(app$get_value(input = "select"), "6")

  app$set_inputs(select = 7)
  app$wait_for_idle()
  expect_equal(app$get_value(input = "select"), "7")
})
