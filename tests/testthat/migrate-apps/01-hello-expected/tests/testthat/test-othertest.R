library(shinytest2)

test_that("Migrated shinytest test: othertest.R", {
  # Comment
  # app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
  app <- AppDriver$new(variant = osName(), seed = 100, shiny_args = list(display.mode = "normal"))

  # expect_pass(shinytest::testApp("../", suffix = osName(), compareImages = TRUE))


  # Another comment
  app$expect_values()

  app$set_inputs(bins = 8)
  app$set_inputs(bins = 9)
  app$expect_values()
  app$expect_screenshot()

  app$set_inputs(bins = 5)
  app$set_inputs(bins = 22)
  app$expect_values()
})
