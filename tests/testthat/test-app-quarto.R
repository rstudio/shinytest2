test_that("Quarto documents can be tested", {
  app <- AppDriver$new(test_path("apps/qmd"), seed = 68850)

  app$set_inputs(name = "barret")
  app$click("greet")
  app$expect_values()

  expect_equal(
    app$get_value(output = "greeting"),
    "Hello barret!"
  )
})
