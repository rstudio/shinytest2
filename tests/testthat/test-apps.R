skip_on_cran() # Uses chromote

test_that("dummy", {
  expect_equal(1, 1)
})

dirs <- dir(test_path("apps"), full.names = TRUE)
lapply(dirs, function(shiny_app_dir) {
  test_path <- file.path(shiny_app_dir, "tests", "")
  if (dir.exists(test_path)) {
    test_app(shiny_app_dir, name = shiny_app_dir)
  }
})

test_that("dummy two", {
  expect_equal(1, 1)
})
