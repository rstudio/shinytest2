
test_that("download files work from link and button", {
  app <- AppDriver$new(variant = NULL)

  app$wait_for_js("$('#download_link_csv').attr('href') != ''")
  app$wait_for_js("$('#download_button_csv').attr('href') != ''")
  app$wait_for_js("$('#download_link_txt').attr('href') != ''")
  app$wait_for_js("$('#download_button_txt').attr('href') != ''")
  app$wait_for_js("$('#download_link_binary').attr('href') != ''")
  app$wait_for_js("$('#download_button_binary').attr('href') != ''")

  app$expect_download("download_link_txt")
  app$expect_download("download_button_txt")

  app$expect_download("download_link_csv", compare = testthat::compare_file_text)
  app$expect_download("download_button_csv", compare = testthat::compare_file_text)

  app$expect_download("download_link_binary", compare = testthat::compare_file_binary)
  app$expect_download("download_button_binary", compare = testthat::compare_file_binary)
})


test_that("download files can be retrieved", {
  on.exit({
    if (fs::file_exists("barret.test")) {
      fs::file_delete("barret.test")
    }
  }, add = TRUE)

  app <- AppDriver$new(variant = NULL)

  app$wait_for_js("$('#download_link_csv').attr('href') != ''")
  app$wait_for_js("$('#download_button_csv').attr('href') != ''")

  link_file <- app$get_download("download_link_csv")
  button_file <- app$get_download("download_button_csv", "barret.test")

  expect_equal(fs::path_file(link_file), "download-link.csv")
  expect_equal(fs::path_file(button_file), "barret.test")

  expect_gt(file.info(link_file)$size, 0)
  expect_gt(file.info(button_file)$size, 0)
})
