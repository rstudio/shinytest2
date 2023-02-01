test_that("JS can take a file or script", {
  app <- AppDriver$new(test_path("apps/hello"))

  app$set_inputs(name = "Hadley")
  app$run_js("window.testVal = 'testLocal';")
  expect_equal(
    app$get_js("window.testVal;"),
    "testLocal"
  )

  app$run_js(file = test_path("apps/hello/js/execute-js.js"))
  expect_equal(
    app$get_js("window.testVal;"),
    "testFile"
  )

  expect_equal(
    app$get_js(file = test_path("apps/hello/js/expect-js.js")),
    "testExpectJs"
  )
  app$expect_js(file = test_path("apps/hello/js/expect-js.js"))

  expect_warning(
    app$get_js("1 + 1", file = test_path("apps/hello/js/one-plus-one.js")),
    "Both `file` and `script` are specified", fixed = TRUE
  )

  expect_error(
    app$wait_for_js("1 +"),
    "Error found while waiting for JavaScript script to"
  )
})
