test_that("basic text and dom outputs are captured", {
  app <- ShinyDriver2$new(test_path("apps/ui-output"), variant = NULL)
  app$setInputs(val = "<div id='custom'><p>My Custom Output</p></div>")

  expect_snapshot_app_text(app, "#text")
  expect_snapshot_app_text(app, "#custom")

  expect_snapshot_app_dom(app, "#custom", outerHTML = FALSE)
  expect_snapshot_app_dom(app, "#custom", outerHTML = TRUE)
})
