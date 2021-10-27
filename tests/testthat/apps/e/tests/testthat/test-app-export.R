
test_that("Exported values", {
  app <- ShinyDriver2$new(test_path("../../."))

  x <- app$get_all_values()
  expect_identical(x$export$x, 1)
  expect_identical(x$export$y, 2)

  app$setInputs(inc = "click")
  app$setInputs(inc = "click")

  x <- app$get_all_values()
  expect_identical(x$export$x, 3)
  expect_identical(x$export$y, 4)
})
