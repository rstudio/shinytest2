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
    test_app(app_dir, name = "custom name 1", quiet = TRUE)
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
  test_app(app_dir, name = "custom name 2", quiet = TRUE)
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
    test_path("apps/wait"),
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
      check_setup = TRUE,
      quiet = TRUE
    ),
    regexp = "is no longer used"
  )
})


# Test quiet parameter
test_that("quiet = FALSE shows deprecation warning when called in test", {
  # The warning should be shown when quiet = FALSE (default)
  expect_warning(
    test_app(
      app_dir = test_path("apps/task-button"),
      quiet = FALSE
    ),
    regexp = "Calling `shinytest2::test_app\\(\\)` within a \\{testthat\\} test has been deprecated"
  )
})

test_that("quiet = TRUE suppresses deprecation warning when called in test", {
  # The warning should NOT be shown when quiet = TRUE
  expect_no_warning(
    test_app(
      app_dir = test_path("apps/task-button"),
      quiet = TRUE
    )
  )
})
