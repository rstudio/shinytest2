skip_on_cran() # CI is good enough

# If testthat is not a `Depends` package, then `testthat` is guarenteed to be loaded when shinytest2 is loaded.
# This will cause confusion when only `library(shinytest2)` is called.
test_that("testthat is a depends package", {
  depends <- as.list(read.dcf(system.file("DESCRIPTION", package = "shinytest2"))[1, ])$Depends
  expect_match(depends, "testthat")
})



test_that("AppDriver can print while working with `missing_arg()` values", {
  app <- AppDriver$new(
    shinyApp("", function(input, output) {}) # nolint: brace_linter
  )
    expect_error(
    utils::capture.output({
      print(app)
    }),
    NA
  )
})
