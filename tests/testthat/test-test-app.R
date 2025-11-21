test_that("app support loading works", {
  skip_if_no_apps()
  expect_false(exists("n"))

  local({
    local_app_support(app_dir = test_path("apps/wait"))

    expect_true(exists("n"))
    expect_equal(n, 750)
  })

  expect_false(exists("n"))
})

test_that("app support loading with_app_support works", {
  skip_if_no_apps()
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
  skip_if_no_apps()
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
  skip_if_no_apps()
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
  skip_if_no_apps()
  # The warning should NOT be shown when quiet = TRUE
  expect_no_warning(
    test_app(
      app_dir = test_path("apps/task-button"),
      quiet = TRUE
    )
  )
})
