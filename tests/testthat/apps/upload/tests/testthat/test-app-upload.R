library(shinytest2)

test_that("Make sure upload can take use a local file", {
  app <- AppDriver$new(name = "upload")

  app$upload_file(file = "cars.csv")
  app$expect_values()
})

test_that("Make sure upload can take use a local file", {
  app <- AppDriver$new(name = "upload")

  expect_error(
    app$upload_file(file = "missing_file.csv"),
    "Missing upload file at path", fixed = TRUE
  )

})
