library(shinytest2)

test_that("Shiny R Markdown documents can test", {
  app <- AppDriver$new(seed = 9767)
  app$set_inputs(name = "barret")
  app$click("greet")
  app$expect_values()
  expect_equal(
    app$get_value(output = "greeting"),
    "Hello barret!"
  )
})
