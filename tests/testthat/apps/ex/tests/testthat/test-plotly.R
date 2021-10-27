
test_that("plotly webgl works", {
  skip_if_not_installed("plotly")
  # TODO-future; Good candidate for fuzzy matching

  app <- ShinyDriver2$new(test_path("../../."), variant = os_name_and_r_version());
  app$wait_for_value("p", iotype = "output", ignore = list(NULL))
  app_expect_appshot(app, items = list(output = "p"), screenshot = TRUE)
})
