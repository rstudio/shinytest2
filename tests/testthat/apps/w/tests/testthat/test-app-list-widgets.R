test_that("list input & output widgets", {
  app <- ShinyDriver2$new(test_path("../../."))
  expect_snapshot(app$listWidgets())
})
