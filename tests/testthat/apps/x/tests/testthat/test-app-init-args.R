test_that("name arg works", {
  app <- AppDriver$new(
    test_path("../../."),
    name = "init"
  )
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_appshot()
  app$expect_appshot(items = list(output = "greeting"))
})
