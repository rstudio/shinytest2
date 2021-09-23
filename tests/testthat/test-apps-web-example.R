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
  app <- ShinyDriver2$new(test_path("apps/website-example"))
  app$setInputs(name = "Hadley")
  app$setInputs(greet = "click")
  app$expectSnapshot()
  app$expectSnapshot(items = list(output = "greeting"))
})


# # Eventual ideal code:
# test_that("prefix works", {
#   st2_test_app("apps/basic") %>%
#     st2_set_inputs(name = "Hadley") %>%
#     st2_set_inputs(greet = "click") %>%
#     st2_expect_snapshot() %>%
#     st2_expect_snapshot(items = list(output = "greeting"))
# })
