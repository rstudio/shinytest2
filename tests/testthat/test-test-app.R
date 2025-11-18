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
    test_app(app_dir, name = "custom name 1")
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
  test_app(app_dir, name = "custom name 2")
)
test_that("after", {
  expect_equal(1, 1)
  expect_equal(1, 1)
  expect_equal(1, 1)
})


test_that("app support loading works", {
  expect_false(exists("n"))

  local({
    local_app_support(app_dir = test_path("apps/wait"))

    expect_true(exists("n"))
    expect_equal(n, 750)
  })

  expect_false(exists("n"))
})

test_that("app support loading with_app_support works", {
  expect_false(exists("n"))

  with_app_support(
    app_dir = test_path("apps/wait"),
    {
      expect_true(exists("n"))
      expect_equal(n, 750)
    }
  )

  expect_false(exists("n"))
})


# Test check_setup deprecation
test_that("check_setup = TRUE shows deprecation warning", {
  lifecycle::expect_deprecated(
    test_app(
      app_dir = test_path("apps/task-button"),
      check_setup = TRUE
    ),
    regexp = "is no longer used"
  )
})
