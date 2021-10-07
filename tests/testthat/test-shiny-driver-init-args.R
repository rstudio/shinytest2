test_that("name arg works", {
  app <- ShinyDriver2$new(
    test_path("apps/website-example"),
    name = "web-ex-name"
  )
  app$setInputs(name = "Hadley")
  app$setInputs(greet = "click")
  expect_snapshot_app(app)
  expect_snapshot_app(app, items = list(output = "greeting"))
})
