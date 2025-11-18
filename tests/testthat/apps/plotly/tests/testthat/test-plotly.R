test_that("plotly webgl works", {
  skip_if_not_installed("plotly")
  skip(
    "webgl doesn't work with chromote; make a different snapshot pre-process test"
  )
  # TODO-future; Good candidate for fuzzy picture matching

  app <- AppDriver$new()

  app$wait_for_value(output = "p", ignore = list(NULL))

  app$expect_values(output = "p", screenshot_args = FALSE)
})
