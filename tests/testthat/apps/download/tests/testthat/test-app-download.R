
test_that("download files work from link and button", {
  app <- ShinyDriver2$new(test_path("../../."), variant = NULL)

  app$waitFor(paste0("$('#download_link').attr('href') != ''"))
  app$waitFor(paste0("$('#download_button').attr('href') != ''"))

  app_expect_download(app, "download_link")
  app_expect_download(app, "download_button")
})
