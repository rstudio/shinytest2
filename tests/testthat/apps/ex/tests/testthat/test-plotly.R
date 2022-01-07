
test_that("plotly webgl works", {
  skip_if_not_installed("plotly")
  # TODO-future; Good candidate for fuzzy picture matching

  app <- AppDriver$new(test_path("../../."), variant = NULL);
  app$wait_for_value("p", ignore = list(NULL))

  app$expect_values(output = "p", screenshot_args = FALSE)
})
