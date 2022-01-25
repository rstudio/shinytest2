test_that("file contensts are converted", {
  info_env <- as.environment(list(app_var = "app"))
  suppressMessages({
    converted_text <- migrate__algo_2('
    # Comment

    # Comment after blank line
    app <- ShinyDriver$public_methods$initialize(
      "../../",
      seed = 100,
      # Internal comment that will be lost
      shinyOptions =
        list(display.mode = "normal")
    )
    app$snapshotInit("mytest")

    # Another comment
    app$setInputs(bins = 8, values_ = FALSE)
    ',
      info_env
    )
  })

  expect_equal(converted_text, '
    # Comment

    # Comment after blank line
app <- ShinyDriver$public_methods$initialize("../../", seed = 100, shinyOptions = list(display.mode = "normal"))

    # Another comment
app$set_inputs(bins = 8)
    '
  )

  expect_equal(info_env$name, "mytest")
})
