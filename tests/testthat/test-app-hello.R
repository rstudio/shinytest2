# # Previous code:
# test_that("it works", {
#   app <- ShinyDriver$new(test_path("../.."))
#   app$snapshotInit("mytest")
#   app$setInputs(name = "Hadley")
#   app$setInputs(greet = "click")
#   app$snapshot()
#   app$snapshot(list(output = "greeting"))
# })

# shinytest2 code using `app$**()`:
test_that("basic website example works using shinytest", {
  skip_if_no_apps()

  app <- AppDriver$new(
    test_path("apps/hello"),
    variant = platform_variant(r_version = FALSE)
  )

  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")

  # Take picture and record inputs / outputs
  app$expect_screenshot()
  app$expect_values()

  # Snapshot some text values
  app$expect_text("#greeting")
  app$expect_html("#greeting", outer_html = TRUE)

  # Only record `output[c("greeting")]`
  app$expect_values(output = "greeting")
})

# shinytest2 code using `app$**()`:
test_that("basic website example works using testthat", {
  skip_if_no_apps()

  app <- AppDriver$new(
    test_path("apps/hello"),
    variant = platform_variant(r_version = FALSE),
    name = "manual"
  )

  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")

  # Take picture and record inputs / outputs
  tmpfile <- tempfile()
  app$get_screenshot(tmpfile)
  expect_snapshot_file(
    tmpfile,
    name = "manual-screenshot.png",
    variant = app$get_variant()
  )

  values <- app$get_values()
  expect_equal(values$output$greeting, "Hello Hadley!")

  # Snapshot some text values
  expect_equal(
    app$get_text("#greeting"),
    "Hello Hadley!"
  )

  expect_equal(
    app$get_html("#greeting", outer_html = TRUE),
    "<div id=\"greeting\" class=\"shiny-text-output shiny-bound-output\" aria-live=\"polite\">Hello Hadley!</div>"
  )
})
