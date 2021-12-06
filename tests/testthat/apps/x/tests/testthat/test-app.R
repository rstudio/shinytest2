# # Previous code:
# test_that("it works", {
#   app <- ShinyDriver$new(test_path("../.."))
#   app$snapshotInit("mytest")
#   app$setInputs(name = "Hadley")
#   app$setInputs(greet = "click")
#   app$snapshot()
#   app$snapshot(list(output = "greeting"))
# })


# Current shinytest2 code using `app$**()`:
test_that("basic website example works", {
  app <- AppDriver$new(variant = platform_variant())
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")

  # Take picture and record inputs / outputs
  app$expect_appshot()

  # Snapshot some text values
  app$expect_text("#greeting")
  app$expect_html("#greeting", outer_html = FALSE)

  # Only record `output[c("greeting")]`
  app$expect_appshot(items = list(output = "greeting"))
})
