library(shinytest2)

test_that("click causes input without binding to update", {
  app <- AppDriver$new(name = "click")
  app$click("click")
  app$click("click")
  app$click("click")
  app$click("click")
  app$expect_values()

  # Shut down this app to try an make CI happier about the next app
  app$stop()
})

test_that("Can update the input without biding individually", {
  app <- AppDriver$new(name = "no-binding")
  app$set_inputs(counter = 1, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(counter = 2, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(counter = 3, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(counter = 4, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_values()
})
