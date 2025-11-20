library(shinytest2)

test_that("basic text and dom outputs are expected", {
  app <- AppDriver$new(test_path("../../"), variant = NULL)

  app$set_inputs(val = "<div id='custom'><p>My Custom Output</p></div>")

  app$expect_text("#text")
  app$expect_text("#custom")

  app$expect_html("#custom", outer_html = TRUE)
  app$expect_html("#custom", outer_html = FALSE)
})

test_that("basic text and dom outputs are captured", {
  app <- AppDriver$new(test_path("../../"), variant = NULL)

  app$set_inputs(val = "<div id='custom'><p>My Custom Output</p></div>")

  expect_equal(
    app$get_text("#text"),
    "<div id='custom'><p>My Custom Output</p></div>"
  )
  expect_equal(
    app$get_text("#custom"),
    "My Custom Output"
  )

  expect_equal(
    app$get_html("#custom", outer_html = TRUE),
    "<div id=\"custom\"><p>My Custom Output</p></div>"
  )
  expect_equal(
    app$get_html("#custom", outer_html = FALSE),
    "<p>My Custom Output</p>"
  )
})
