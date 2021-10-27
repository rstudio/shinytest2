test_that("name arg works", {
  app <- ShinyDriver2$new(
    test_path("../../."),
    name = "init"
  )
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app_expect_appshot(app)
  app_expect_appshot(app, items = list(output = "greeting"))
})
