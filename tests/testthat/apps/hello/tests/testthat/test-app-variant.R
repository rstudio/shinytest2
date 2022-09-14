# shinytest2 code using `app$**()`:
test_that("screenshots need a variant", {
  app <- AppDriver$new()

  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")

  # Can save values. Variant acts like `NULL`
  app$expect_values()

  expect_error(
    app$expect_screenshot(),
    "can not call `$expect_screenshot()`", fixed = TRUE
  )
})

# Tests where the variant is supplied (both `NULL` and `platform_variant()`) are done in many other files.
