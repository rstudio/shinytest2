
test_that("plotly webgl works", {
  skip_if_not_installed("plotly")
  # TODO-future; Good candidate for fuzzy matching

  app <- AppDriver$new(test_path("../../."), variant = os_name_and_r_version());
  app$wait_for_value("p", iotype = "output", ignore = list(NULL))
  app$expect_appshot(items = list(output = "p"), screenshot = TRUE)
})
