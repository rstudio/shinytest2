
test_that("App returns value from script", {
  app <- AppDriver$new()
  app$click("button")
  app$wait_for_idle() # Wait for the app to stop

  meaning_of_life <- app$stop()
  expect_equal(meaning_of_life, 42)
})

test_that("App returns value from script", {
  app <- AppDriver$new()
  # app$click("button")

  normal_return <- app$stop()
  expect_equal(normal_return, NULL)
})
