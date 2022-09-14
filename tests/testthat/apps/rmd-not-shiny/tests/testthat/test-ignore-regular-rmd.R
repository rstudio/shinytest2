library(shinytest2)

test_that("Regular Rmd files are ignored", {
  app <- AppDriver$new()

  expect_equal(
    app$get_value(output = "format_type"),
    "PDF"
  )
})
