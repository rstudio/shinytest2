library(shinytest2)

test_that("Quarto documents can test", {
  app <- AppDriver$new(seed = 68850)
  withr::defer(app$stop())

  app$set_inputs(name = "barret")
  app$click("greet")
  app$expect_values()
  expect_equal(
    app$get_value(output = "greeting"),
    "Hello barret!"
  )
})
