library(shinytest2)


test_that("`export`ed `plot_obj` is updated by `n`", {

  app <- AppDriver$new()

  expect_n_and_plot <- function(n) {
    # Verify `n` input equals `n`
    n_val <- app$get_value(input = "n")
    testthat::expect_equal(n_val, n, expected.label = n)
    # Verify `dt()` data is first `n` lines of `cars`
    dt <- app$get_value(export = "dt")
    testthat::expect_equal(dt, head(cars, n), expected.label = paste0("head(cars, ", n, ")"))

    # Verify `plot_obj()` data is `dt()`
    plot_obj <- app$get_value(export = "plot_obj")
    testthat::expect_equal(plot_obj$data, dt, info = paste0("n = ", n))
    # Verify `plot_obj()` is consistent
    vdiffr::expect_doppelganger(paste0("cars-points-", n), plot_obj)
  }

  expect_n_and_plot(10)

  app$set_inputs(n = 20)
  expect_n_and_plot(20)
})
