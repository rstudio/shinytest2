
test_that("Exported values", {
  app <- ShinyDriver2$new(test_path("../../."))

  x <- app$get_all_values()
  expect_identical(x$export$x, 1)
  expect_identical(x$export$y, 2)

  app$set_inputs(inc = "click")
  app$set_inputs(inc = "click")

  x <- app$get_all_values()
  expect_identical(x$export$x, 3)
  expect_identical(x$export$y, 4)
})
