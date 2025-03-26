# Empty app to test window sizes
shiny_app <- shiny::shinyApp("", function(input, output) {}) # nolint: brace_linter


test_that("Init and Set window size", {

  app <- AppDriver$new(
    shiny_app,
    name = "set",
    variant = NULL,
    height = 100,
    width = 150
  )

  expect_equal(app$get_js("let size = {height: window.innerHeight, width: window.innerWidth}; size"), list(height = 100, width = 150))

  app$set_window_size(height = 200, width = 250)
  expect_equal(app$get_js("let size = {height: window.innerHeight, width: window.innerWidth}; size"), list(height = 200, width = 250))
})


test_that("AppDriver height and width must be positive numbers", {
  expect_error(AppDriver$new(shiny_app, height = 100, width = "150"))
  expect_error(AppDriver$new(shiny_app, height = "100", width = 150))

  expect_error(AppDriver$new(shiny_app, height = 0, width = 150))
  expect_error(AppDriver$new(shiny_app, height = 100, width = 0))
})
