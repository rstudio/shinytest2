skip_on_cran() # Uses chromote

test_that("before", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})

dirs <- Filter(
  dir(test_path("apps"), full.names = TRUE),
  f = function(x) dir.exists(file.path(x, "tests", ""))
) %>% head(2)
lapply(dirs, function(shiny_app_dir) {
  test_app(shiny_app_dir)
})

test_that("after", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})
