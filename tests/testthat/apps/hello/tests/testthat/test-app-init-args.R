test_that("name arg works", {
  app <- AppDriver$new(
    test_path("../../."),
    name = "test",
    variant = NULL
  )
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_values(output = "greeting", screenshot_args = FALSE)
  app$expect_values(output = "greeting", screenshot_args = FALSE, name = "custom")
})
