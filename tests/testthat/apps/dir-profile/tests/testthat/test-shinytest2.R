library(shinytest2)

test_that("{shinytest2} recording: dir-profile", {
  app <- AppDriver$new(name = "dir-profile", height = 1133, width = 1282)

  expect_equal(
    app$get_value(output="txt"),
    "found!"
  )
})
