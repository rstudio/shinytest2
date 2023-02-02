test_that("{shinytest2} recording: kgs", {
  app <- AppDriver$new(
    test_path("apps/files-server-ui"),
    name = "kgs",
    height = 1321,
    width = 1221
  )
  app$set_inputs(kg = 100)
  app$expect_values()
})


test_that("server.R and app.R are not compatible", {
  expect_error(
    AppDriver$new(test_path("apps/files-app-server"))$stop(),
    "Unintented behavior may occur",
    fixed = TRUE
  )
})


test_that("rmarkdown and app.R are not compatible", {
  expect_error(
    AppDriver$new(test_path("apps/files-app-rmd")),
    "`app_dir` must be a directory containing",
    fixed = TRUE
  )
})
