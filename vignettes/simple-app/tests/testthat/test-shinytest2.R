library(shinytest2)

test_that("{shinytest2} recording: simple-app", {
  app <- AppDriver$new(name = "simple-app", height = 407, width = 348)
  app$set_inputs(name = "Hadley")
  app$click("greet")
  app$expect_values()
})
