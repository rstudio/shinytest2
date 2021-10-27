test_that("basic text and dom outputs are captured", {
  app <- ShinyDriver2$new(test_path("../../."), variant = NULL)
  app$set_inputs(val = "<div id='custom'><p>My Custom Output</p></div>")

  app_expect_text(app, "#text")
  app_expect_text(app, "#custom")

  app_expect_html(app, "#custom", outer_html = FALSE)
  app_expect_html(app, "#custom", outer_html = TRUE)
})
