library(shinytest2)

test_that("Initial Shiny values are consistent", {
  app <- AppDriver$new()

  app$expect_values()
})
