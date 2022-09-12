library(shinytest2)

test_that("`export`ed `plot_obj` is updated by `n`", {
  skip_if_not_installed("vdiffr")

  app <- AppDriver$new()
  withr::defer(app$stop())

  # Verify `dt()` uses first 10 lines of `cars`
  n10 <- app$get_value(input = "n")
  expect_equal(n10, 10)
  # Verify `dt10()` data is first 10 lines of `cars`
  dt10 <- app$get_value(export = "dt")
  expect_equal(dt10, head(cars, n10))

  # Verify `plot_obj()` data is `dt()`
  plot_obj_10 <- app$get_value(export = "plot_obj")
  expect_equal(plot_obj_10$data, dt10)
  # Verify `plot_obj()` is consistent
  vdiffr::expect_doppelganger("cars-points-10", plot_obj_10)

  ## Update `n` to 20
  app$set_inputs(n = 20)

  # Verify `n` was updated
  n20 <- app$get_value(input = "n")
  expect_equal(n20, 20)
  # Verify `dt()` uses first 20 lines of `cars`
  dt20 <- app$get_value(export = "dt")
  expect_equal(dt20, head(cars, n20))

  # Verify `plot_obj()` data is `dt()`
  plot_obj_20 <- app$get_value(export = "plot_obj")
  expect_equal(plot_obj_20$data, dt20)
  vdiffr::expect_doppelganger("cars-points-20", plot_obj_20)
})
