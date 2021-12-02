test_that("basic text and dom outputs are captured", {
  app <- AppDriver$new(test_path("../../."), variant = NULL)
  app$set_inputs(val = "<div id='custom'><p>My Custom Output</p></div>")

  app$expect_text("#text")
  app$expect_text("#custom")

  app$expect_html("#custom", outer_html = FALSE)
  app$expect_html("#custom", outer_html = TRUE)
})
