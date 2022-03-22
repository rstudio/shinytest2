

test_that("Snapshots with the same name are found", {
  shiny_app <- shiny::shinyApp(shiny::h1("Empty App"), function(input, output) { })

  app <- AppDriver$new(shiny_app, name = NULL, expect_values_screenshot_args = FALSE)
  app$expect_values()

  app2 <- AppDriver$new(shiny_app, name = NULL, expect_values_screenshot_args = FALSE)
  expect_error(
    app2$expect_values(),
    "Duplicate snapshot files detected"
  )
})
