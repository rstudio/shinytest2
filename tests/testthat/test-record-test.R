test_that("record_test_file_name() handles character paths correctly", {
  skip_on_cran()

  # Should generate name from path when record_in_package = TRUE
  result <- record_test_file_name(".", TRUE)
  expect_true(grepl("^test-app-.*\\.R$", result))

  result <- record_test_file_name("/some/path/to/app", TRUE)
  expect_equal(result, "test-app-app.R")

  # Should use default name when record_in_package = FALSE
  result <- record_test_file_name(".", FALSE)
  expect_equal(result, "test-shinytest2.R")
})

test_that("record_test_file_name() handles functions correctly (issue #439)", {
  skip_on_cran()

  # Previously this would error with "object of type 'closure' is not subsettable"
  # when fs::path_file() was called on a function

  app_fn <- function() {}

  # Should fall back to default name regardless of record_in_package
  result <- record_test_file_name(app_fn, TRUE)
  expect_equal(result, "test-shinytest2.R")

  result <- record_test_file_name(app_fn, FALSE)
  expect_equal(result, "test-shinytest2.R")
})

test_that("record_test_file_name() handles shiny.appobj correctly", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output) {}
  )

  # Should fall back to default name for app objects
  result <- record_test_file_name(app, TRUE)
  expect_equal(result, "test-shinytest2.R")

  result <- record_test_file_name(app, FALSE)
  expect_equal(result, "test-shinytest2.R")
})

test_that("record_test_file_name() handles AppDriver objects correctly", {
  skip_on_cran()

  # Create a mock AppDriver-like object
  # We can't easily create a real AppDriver in tests, so we create a mock
  mock_driver <- structure(list(), class = "AppDriver")

  # Should fall back to default name for AppDriver objects
  result <- record_test_file_name(mock_driver, TRUE)
  expect_equal(result, "test-shinytest2.R")

  result <- record_test_file_name(mock_driver, FALSE)
  expect_equal(result, "test-shinytest2.R")
})
