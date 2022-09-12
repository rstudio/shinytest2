library(shinytest2)

test_that("{shinytest2} recording: kgs", {
  app <- AppDriver$new(name = "kgs", height = 1321, width = 1221)
  app$set_inputs(kg = 100)
  app$expect_values()
})
