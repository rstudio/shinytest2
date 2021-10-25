
test_that("Exported values", {
  app <- ShinyDriver2$new(test_path("apps/download/"), variant = NULL)

  app$waitForShiny()

  app_expect_download(app, "download_link")
  app_expect_download(app, "download_button")
})
