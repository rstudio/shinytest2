
test_that("download files work from link and button", {
  app <- AppDriver$new(variant = NULL)

  app$wait_for_script(paste0("$('#download_link').attr('href') != ''"))
  app$wait_for_script(paste0("$('#download_button').attr('href') != ''"))

  app$expect_download("download_link")
  app$expect_download("download_button")
})
