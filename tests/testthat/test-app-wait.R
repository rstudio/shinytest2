# Testing that the app helper files can be loaded
test_that("wait for idle works", {
  skip_if_no_apps()

  local_app_support(test_path("apps/wait"))
  app <- AppDriver$new(test_path("apps/wait"))

  app$wait_for_idle(duration = 2 * n)

  expect_equal(app$get_value(output = "txt"), "1 2 3")
})

test_that("waiting a lesser value will not be enough", {
  skip_if_no_apps()

  local_app_support(test_path("apps/wait"))
  app <- AppDriver$new(test_path("apps/wait"))

  app$wait_for_idle(duration = n / 2)

  expect_failure(
    expect_equal(app$get_value(output = "txt"), "1 2 3")
  )
})
