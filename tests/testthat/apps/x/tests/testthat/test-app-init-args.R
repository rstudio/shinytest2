test_that("name arg works", {
  app <- AppDriver$new(
    test_path("../../."),
    variant = NULL
  )
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_values(output = "greeting", screenshot_args = FALSE)
})
