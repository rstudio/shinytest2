test_that("name arg works", {
  app <- ShinyDriver2$new(
    test_path("apps/website-example"),
    name = "web-ex-name"
  )
  app$setInputs(name = "Hadley")
  app$setInputs(greet = "click")
  app_expect_appshot(app)
  app_expect_appshot(app, items = list(output = "greeting"))
})
