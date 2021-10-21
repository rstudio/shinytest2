
test_that("Exported values", {
  app <- ShinyDriver2$new(test_path("apps/download/"))

  app$waitForShiny()

  # app_expect_appshot(app)
  # app$waitForValue("download_link", iotype = "output", ignore = list("", NULL))

  app_expect_download(app, "download_link")
  app_expect_download(app, "download_button")
})
