test_that("basic website example works", {
  app <- ShinyDriver2$new(test_path("../../."), variant = NULL)
  app$setInputs(name = "Hadley")
  app$setInputs(greet = "click")
  app_expect_appshot(app, items = list(output = "greeting")) # Hadley
  app$setInputs(name = "Barret")
  app$click("greet")
  app_expect_appshot(app, items = list(output = "greeting")) # Barret
})
