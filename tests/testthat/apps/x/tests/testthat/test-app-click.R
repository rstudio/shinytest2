test_that("basic website example works", {
  app <- ShinyDriver2$new(test_path("../../."), variant = NULL)
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app_expect_appshot(app, items = list(output = "greeting")) # Hadley
  app$set_inputs(name = "Barret")
  app$click("greet")
  app_expect_appshot(app, items = list(output = "greeting")) # Barret
})
