
test_that("Init and Set window size", {

  app <- AppDriver$new(
    name = "set",
    variant = NULL,
    height = 100,
    width = 150
  )
  expect_equal(app$execute_js("return {height: window.innerHeight, width: window.innerWidth}"), list(height = 100, width = 150))

  app$set_window_size(height = 200, width = 250)
  expect_equal(app$execute_js("return {height: window.innerHeight, width: window.innerWidth}"), list(height = 200, width = 250))
})


test_that("AppDriver height and width must be positive numbers", {
  expect_error(AppDriver$new(height = 100, width = "150"))
  expect_error(AppDriver$new(height = "100", width = 150))

  expect_error(AppDriver$new(height = 0, width = 150))
  expect_error(AppDriver$new(height = 100, width = 0))
})
