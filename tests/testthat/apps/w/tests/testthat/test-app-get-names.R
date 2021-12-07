test_that("list current input & output component names", {
  app <- AppDriver$new(test_path("../../."))
  expect_snapshot(app$get_names())
})
