test_that("$get_value errors are caught", {

  app <- AppDriver$new()
  expect_error(
    app$get_value("txt"),
  )

  expect_error(
    app$get_value(input = "something", output = "txt"),
    "specify only one"
  )
  expect_error(
    app$get_value(input = "something", export = "txt"),
    "specify only one"
  )
  expect_error(
    app$get_value(output = "something", export = "txt"),
    "specify only one"
  )
  expect_error(
    app$get_value(output = c("A", "B")),
    "length 2"
  )
  expect_error(
    app$get_value(output = character(0)),
    "length 0"
  )


  expect_error(
    app$wait_for_value(input = "something", output = "txt"),
    "specify only one"
  )
  expect_error(
    app$wait_for_value(input = "something", export = "txt"),
    "specify only one"
  )
  expect_error(
    app$wait_for_value(output = "something", export = "txt"),
    "specify only one"
  )
  expect_error(
    app$wait_for_value(output = c("A", "B")),
    "length 2"
  )
  expect_error(
    app$wait_for_value(output = character(0)),
    "length 0"
  )
})




test_that("wait for value works on output", {

  app <- AppDriver$new()

  expect_equal(
    app$wait_for_value(output = "txt"),
    "1 2 3"
  )
})

test_that("wait for value works on input", {

  app <- AppDriver$new()
  expect_equal(
    app$wait_for_value(input = "slider3"),
    3
  )
})
