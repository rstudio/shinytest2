test_that("list input & output widgets", {
  app <- AppDriver$new(test_path("../../."))
  expect_snapshot(app$list_widgets())
})
