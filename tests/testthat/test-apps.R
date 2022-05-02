skip_on_cran() # Uses chromote

# Test reporter displays info
# Similar to
# v |         4 | apps
# v |         1 | apps - custom name - shinytest2 [0.3s]
# ✔ |         4 | apps
test_that("before", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})
test_that("wrapper", {
  expect_equal(1, 1)
  test_app(test_path("apps/files-app-rmd"), name = "custom name")
  expect_equal(1, 1)
})
test_that("after", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})


## --------------------------------


# Test all apps work as expected
dirs <- Filter(
  dir(test_path("apps"), full.names = TRUE),
  f = function(x) dir.exists(file.path(x, "tests", ""))
)
lapply(dirs, function(shiny_app_dir) {
  test_app(shiny_app_dir)
})
