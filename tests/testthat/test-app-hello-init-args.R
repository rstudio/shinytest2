test_that("name arg works", {
  skip_if_no_apps()

  app <- AppDriver$new(
    test_path("apps/hello"),
    name = "test",
    variant = NULL
  )

  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_values(output = "greeting", screenshot_args = FALSE)
  app$expect_values(
    output = "greeting",
    screenshot_args = FALSE,
    name = "custom"
  )
})
