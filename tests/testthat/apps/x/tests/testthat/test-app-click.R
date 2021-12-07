test_that("basic website example works", {
  app <- AppDriver$new(test_path("../../."), variant = NULL)
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_appshot(items = list(output = "greeting")) # Hadley
  app$set_inputs(name = "Barret")
  app$click("greet")
  app$expect_appshot(items = list(output = "greeting")) # Barret
})
