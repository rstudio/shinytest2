
test_that("plotly webgl works", {
  skip_if_not_installed("plotly")
  app <- ShinyDriver2$new(test_path("apps/plotly-webgl"), variant = NULL); # TODO - future; Good candidate for fuzzy matching
  app$waitForValue("p", iotype = "output", ignore = list(NULL))
  app$expectSnapshot(items = list(output = "p"), screenshot = TRUE)
})
