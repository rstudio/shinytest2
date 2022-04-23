
test_that("AppDriver can receive a shiny.obj object", {

  b_app <- source("apps/hello/app.R")$value
  app <- AppDriver$new(b_app, name = "app", expect_values_screenshot_args = FALSE)

  app$set_inputs(name = "Barret")
  app$click("greet")

  app$expect_values()

  expect_true(fs::path_has_parent(app$get_dir(), tempdir()))

  # Shut down this app to try an make CI happier about the next app
  app$stop()
})

test_that("AppDriver can receive a shinyAppDir object", {

  b_app <- shiny::shinyAppDir("apps/hello/")

  app <- AppDriver$new(b_app, name = "app-dir", expect_values_screenshot_args = FALSE)

  app$set_inputs(name = "Barret")
  app$click("greet")

  app$expect_values()

  expect_true(fs::path_has_parent(app$get_dir(), tempdir()))
})
