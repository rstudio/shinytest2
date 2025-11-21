test_that("Shiny R Markdown documents can test", {
  skip_if_no_apps()

  app <- AppDriver$new(test_path("apps/rmd-shiny"), seed = 9767, name = "shiny")

  app$set_inputs(name = "barret")
  app$click("greet")
  app$expect_values(input = TRUE, output = "greeting")
  expect_equal(
    app$get_value(output = "greeting"),
    "Hello barret!"
  )
})


test_that("Prerendered Shiny R Markdown documents can test", {
  skip_if_no_apps()

  app <- AppDriver$new(test_path("apps/rmd-pre"), seed = 9767, name = "pre")

  app$set_inputs(name = "barret")
  app$click("greet")
  app$expect_values()
  expect_equal(
    app$get_value(output = "greeting"),
    "Hello barret!"
  )
})


test_that("Regular Rmd files are ignored", {
  skip_if_no_apps()

  app <- AppDriver$new(
    test_path("apps/rmd-not-shiny"),
    seed = 9767,
    name = "not"
  )

  expect_equal(
    app$get_value(output = "format_type"),
    "PDF"
  )
})
