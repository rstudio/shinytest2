
test_that("App returns value from script", {
  app <- AppDriver$new()
  app$click("button")

  meaning_of_life <- app$stop()
  expect_equal(meaning_of_life, 42)
})
