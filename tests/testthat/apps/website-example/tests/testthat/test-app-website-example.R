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
  app <- ShinyDriver2$new(test_path("../../."))
  app$setInputs(name = "Hadley")
  app$setInputs(greet = "click")

  # Take picture and record inputs / outputs
  app_expect_appshot(app)

  # app$expectDOM("#greet")
  # app$expectHtml()

  # Only record `output[c("greenting")]`
  app_expect_appshot(app, items = list(output = "greeting"))
})


# # Eventual ideal code:
# test_that("prefix works", {
#   testing_app("apps/basic") %>%
#     app_set_inputs(name = "Hadley") %>%
#     app_set_inputs(greet = "click") %>%
#     app_expect_appshot() %>%
#     app_expect_appshot(items = list(output = "greeting"))
# })
