library(shinytest2)

test_that("server.R and app.R are not compatible", {
  expect_error(
    AppDriver$new()$stop(),
    "Unintented behavior may occur",
    fixed = TRUE
  )
})
