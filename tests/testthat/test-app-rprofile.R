test_that("{shinytest2} recording: dir-profile", {
  skip_if_no_apps()

  app <- AppDriver$new(
    test_path("apps/dir-rprofile"),
    name = "dir-profile",
    height = 1133,
    width = 1282
  )

  expect_equal(
    app$get_value(output = "txt"),
    "found!"
  )
})
