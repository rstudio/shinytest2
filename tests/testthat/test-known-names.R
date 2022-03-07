
test_that("known names are being found", {
  tmpfile <- tempfile()
  withr::defer({
    if (fs::file_exists(tmpfile)) {
      fs::file_delete(tmpfile)
    }
  })
  cat(file = tmpfile, "
  AppDriver$new()
  AppDriver$new(name = NULL)
  shinytest2::AppDriver$new(name = 'test')
  shinytest2:::AppDriver$new(name = 'test2')
  some_value <- 'value'
  AppDriver$new(name = some_value)
  ")

  expect_equal(
    known_app_driver_name_values(tmpfile),
    list(NULL, NULL, "test", "test2")
  )
})
