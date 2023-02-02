app_dir <- test_path("apps/files-app-rmd")
if (!dir.exists(file.path(app_dir, "tests", "testthat"))) {
  # skip("App test folders have been ignored")
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
  expect_error(
    test_app(app_dir, name = "custom name 1"),
    "This should be an error"
  )
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
expect_error(
  test_app(app_dir, name = "custom name 2"),
  "This should be an error"
)
test_that("after", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})
