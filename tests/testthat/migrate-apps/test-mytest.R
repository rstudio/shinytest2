test_that("mytest.R", {
  # Comment
  # app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
  app <- AppDriver$new(variant = osName(), name = "mytest", seed = 100,
    shiny_args = list(display.mode = "normal"))

  # Another comment
  app$expect_values()

  app$set_inputs(bins = 8)
  app$set_inputs(bins = 9)
  app$expect_values()

  app$set_inputs(bins = 5)
  app$set_inputs(bins = 22)
  app$expect_values()
})


test_that("otherTest.R", {
  # Comment
  # app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
  app <- AppDriver$new(variant = osName(), name = "otherTest", seed = 100,
    shiny_args = list(display.mode = "normal"))

  # Another comment
  app$expect_values()

  app$set_inputs(bins = 8)
  app$set_inputs(bins = 9)
  app$expect_values()

  app$set_inputs(bins = 5)
  app$set_inputs(bins = 22)
  app$expect_values()


  expect_equal(app$getValue("someInput"), someValue)
  # takes a debug screenshot
  app$expect_values(input = "someInput")

  app$expect_screenshot()

})
