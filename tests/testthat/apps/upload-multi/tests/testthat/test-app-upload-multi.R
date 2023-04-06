library(shinytest2)

test_that("Make sure upload can use two local files", {
  app <- AppDriver$new(name = "upload-multi")

  app$upload_file(files = c("cars.csv", "cars_2.csv"))
  app$expect_values()

  expect_error(
    app$upload_file(files = "missing_file.csv"),
    "Error finding upload file at path", fixed = TRUE
  )
})
