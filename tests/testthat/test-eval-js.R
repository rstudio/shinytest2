require("shiny", quietly = TRUE, character.only = TRUE)

test_that("Duplicate input ids are found", {
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


  app <- AppDriver$new(shiny_app, view = TRUE)
  withr::defer(app$stop())

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
