app_dir <- test_path("apps/files-app-rmd")
if (!dir.exists(file.path(app_dir, "tests", "testthat"))) {
  skip("App test folders have been ignored")
}

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
  test_app(app_dir, name = "custom name 1")
  expect_equal(1, 1)
})
test_that("after", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})

test_that("before", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})
test_app(app_dir, name = "custom name 2")
test_that("after", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})


## --------------------------------


# Test all apps work as expected
lapply(
  dir(test_path("apps"), full.names = TRUE),
  function(shiny_app_dir) {
    test_app(shiny_app_dir)
  }
)
