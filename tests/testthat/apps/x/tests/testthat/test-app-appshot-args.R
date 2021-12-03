test_that("name arg works", {
  app <- AppDriver$new(
    test_path("../../."),
    name = "init",
    screenshot_args = FALSE,
    variant = FALSE
  )
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_appshot() # should NOT produce a screenshot
  app$expect_appshot(items = list(output = "greeting")) # should NOT produce a screenshot
  expect_error(
    app$expect_appshot(items = FALSE),
    "can not be `FALSE`", fixed = TRUE
  )

  # should NOT produce a componentshot
  app$expect_appshot(items = FALSE, screenshot = list(selector = "#custom_div"))
})

test_that("items can be many values", {
  app <- AppDriver$new(test_path("../../."), variant = os_name_and_r_version())
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_appshot(items = FALSE) # no componentsshot
  app$expect_appshot(items = TRUE, screenshot = FALSE) # no screenshot
  app$expect_appshot(items = list(output = "greeting")) # only square screenshot; componentshot
})
