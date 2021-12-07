
# If testthat is not a `Depends` package, then `testthat` is guarenteed to be loaded when shinytest2 is loaded.
# This will cause confusion when only `library(shinytest2)` is called.
test_that("testthat is a depends package", {
  depends <- as.list(read.dcf(system.file("DESCRIPTION", package = "shinytest2"))[1, ])$Depends
  expect_match(depends, "testthat")
})
