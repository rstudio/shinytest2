require("shiny", quietly = TRUE, character.only = TRUE)

shiny_app <- shinyApp(
  ui = fluidPage(
    "Eval JS",
    tags$script("
      var test_value = 42;
      var test_string = 'Hello world!';
    "),
    verbatimTextOutput("txt")
  ),
  server = function(input, output) {
    # empty
    output$txt <- renderText({
      "(app loaded)"
    })
  }
)



test_that("Duplicate input ids are found", {
  app <- AppDriver$new(shiny_app)

  app$wait_for_js("test_value === 42")
  app$wait_for_js("let a = test_value; let b = 42; a === b")

  app$wait_for_js("test_value === 42;")
  app$wait_for_js("test_string != '\"';")
  app$wait_for_js("test_string !== \"'\";")
  app$wait_for_js("test_string === 'Hello world!';")

  # https://github.com/rstudio/shinytest2/issues/236
  app$wait_for_js("'test'==='test'")
  app$wait_for_js('"test"==="test"')

  # Provide a test
  # Errors will be thrown above if the conditions can not be met
  expect_true(TRUE)
})


test_that("Errors are caught in $wait_for_js()", {
  app <- AppDriver$new(shiny_app)

  expect_error(
    app$wait_for_js("test_value === /"),
    "test_value === /"
  )

  expect_error(
    app$wait_for_js("false", timeout = 100),
    "Timed out waiting for JavaScript script to return `true`"
  )
})
