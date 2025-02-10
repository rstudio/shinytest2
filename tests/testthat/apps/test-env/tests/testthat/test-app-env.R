test_that("local pkg env is loaded", {
  expect_equal(internal_shinytest2_value, TRUE)

  AppDriver$new(variant = NULL)
})
