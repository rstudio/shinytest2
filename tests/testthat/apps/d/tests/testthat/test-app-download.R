
test_that("download files work from link and button", {
  app <- AppDriver$new(test_path("../../."), variant = NULL)

  app$wait_for_condition(paste0("$('#download_link').attr('href') != ''"))
  app$wait_for_condition(paste0("$('#download_button').attr('href') != ''"))

  app$expect_download("download_link")
  app$expect_download("download_button")
})
