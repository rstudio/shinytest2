# Related https://github.com/rstudio/shinytest2/issues/388
# Reprex altered from https://stackoverflow.com/questions/78517385/testing-async-extendedtask-with-shinytest2
# 2 issues from reprex:
# * $click() did not work as task button preprocessor wasn't registered
# * Asserting the value of the resolved async output needs to happen after the output value updates due to timeing issues
test_that("bslib task button", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("bslib")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")


  app <- AppDriver$new()

  app$click("run_normal")
  expect_equal(app$get_value(export = "normal"), 10)

  # Get current value
  cur_async_value <- app$get_value(export = "async")
  app$click("run_async")
  # Wait until expected output changes
  new_async_value <- app$wait_for_value(export = "async", ignore = list(cur_async_value))
  # Assert new value
  expect_equal(new_async_value, 10)
})
