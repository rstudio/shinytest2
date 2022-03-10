library(shinytest2)

test_that("Migrated shinytest test: mytest.R", {
  app <- AppDriver$new(variant = shinytest2::platform_variant(),
    seed = 100, shiny_args = list(display.mode = "normal"))

  app$set_inputs(decimal = 1)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(range = c(200, 1000))
  app$set_inputs(format = 7500)
  app$set_inputs(animation = 1051)
  app$expect_values()
  app$expect_screenshot()
})
