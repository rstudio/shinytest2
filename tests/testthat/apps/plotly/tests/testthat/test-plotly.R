
test_that("plotly webgl works", {
  skip_if_not_installed("plotly")
  # TODO-future; Good candidate for fuzzy picture matching

  app <- AppDriver$new()

  app$wait_for_value(output = "p", ignore = list(NULL))

  app$expect_values(output = "p", screenshot_args = FALSE)
})
