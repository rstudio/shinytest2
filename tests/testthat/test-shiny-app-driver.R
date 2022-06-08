library(shiny)


hello_app <- shinyApp(
  ui = fluidPage(
    textInput("name", "What is your name?"),
    actionButton("greet", "Greet"),
    textOutput("greeting"),
    tags$br(),
    # consistenly sized box and consistent across OS
    tags$div(
      id = "custom_div",
      style = "width: 100px;height: 100px;"
    )
  ),
  server = function(input, output, session) {
    output$greeting <- renderText({
      shiny::req(input$greet)
      paste0("Hello ", shiny::isolate(input$name), "!")
    })
  }
)


test_that("AppDriver can receive a shiny.obj object", {

  app <- AppDriver$new(hello_app, name = "app", expect_values_screenshot_args = FALSE)

  app$set_inputs(name = "Barret")
  app$click("greet")

  app$expect_values()

  expect_true(fs::path_has_parent(app$get_dir(), tempdir()))

  # Shut down this app to try an make CI happier about the next app
  app$stop()
})

test_that("AppDriver can receive a shinyAppDir object", {

  app <- AppDriver$new(hello_app, name = "app-dir", expect_values_screenshot_args = FALSE)

  app$set_inputs(name = "Barret")
  app$click("greet")

  app$expect_values()

  expect_true(fs::path_has_parent(app$get_dir(), tempdir()))
})
