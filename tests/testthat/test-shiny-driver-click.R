test_that("basic website example works", {
  app <- ShinyDriver2$new(test_path("apps/website-example"), variant = NULL)
  app$setInputs(name = "Hadley")
  app$setInputs(greet = "click")
  app$expectSnapshot(items = list(output = "greeting")) # Hadley
  app$setInputs(name = "Barret")
  app$click("greet")
  app$expectSnapshot(items = list(output = "greeting")) # Barret
})
