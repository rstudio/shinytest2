test_that("App dir .Renviron is loaded", {
  skip_if_no_apps()

  app <- AppDriver$new(test_path("apps/dir-renviron"))

  expect_equal(
    app$get_value(output = "txt"),
    "found!"
  )
})
