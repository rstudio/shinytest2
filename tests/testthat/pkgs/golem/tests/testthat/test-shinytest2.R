library(shinytest2)

test_that("{golem} apps don't need to be installed", {
  app <- AppDriver$new("app.R", name = "golem-app")
  app$wait_for_idle()

  expect_equal(
    app$get_value(output = "text_output"),
    "This is a test output"
  )
})
