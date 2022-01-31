library(shinytest2)

test_that("Migrated shinytest test: mytest.R", {
  # Comment
  # app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
  app <- AppDriver$new(seed = 100, shiny_args = list(display.mode = "normal"))


  # Another comment
  # Only values
  app$expect_values()

  app$set_inputs(bins = 8)
  app$set_inputs(bins = 9)
  # Values and screenshot
  app$expect_values()
  app$expect_screenshot()

  app$set_inputs(bins = 5)
  app$set_inputs(bins = 22)
  # Only values
  app$expect_values()
})
