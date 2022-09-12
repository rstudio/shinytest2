library(shinytest2)

test_that("Make sure upload can use a local file", {
  app <- AppDriver$new(name = "upload")
  withr::defer(app$stop())

  app$upload_file(file = "cars.csv")
  app$expect_values()

  expect_error(
    app$upload_file(file = "missing_file.csv"),
    "Error finding upload file at path", fixed = TRUE
  )
})
