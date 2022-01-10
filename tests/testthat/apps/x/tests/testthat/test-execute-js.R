test_that("JS can take a file or script", {
  app <- AppDriver$new()

  app$set_inputs(name = "Hadley")
  app$execute_js("window.testVal = 'testLocal';")
  expect_equal(
    app$execute_js("return window.testVal;"),
    "testLocal"
  )

  app$execute_js(file = test_path("js/execute-js.js"))
  expect_equal(
    app$execute_js("return window.testVal;"),
    "testFile"
  )

  expect_equal(
    app$execute_js(file = test_path("js/expect-js.js")),
    "testExpectJs"
  )
  app$expect_js(file = test_path("js/expect-js.js"))

  expect_warning(
    app$execute_js("return 1 + 1", file = test_path("js/one-plus-one.js")),
    "Both `file` and `script` are specified", fixed = TRUE
  )

  expect_error(
    app$wait_for_js("1 +"),
    "Error found while waiting for JavaScript script to"
  )
  expect_error(
    app$wait_for_js("'not returned'", timeout = 100, interval = 0),
    "Timed out waiting for JavaScript"
  )
})
