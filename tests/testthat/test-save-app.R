require("shiny", quietly = TRUE, character.only = TRUE)

test_that("Saving an app has the right context", {
  n <- 5
  shiny_app <- shinyApp(
    ui = fluidPage(
      textOutput("text")
    ),
    server = function(input, output) {
      output$text <- renderText({
        n
      })
    }
  )

  app <- AppDriver$new(shiny_app)

  expect_equal(
    app$get_value(output = "text"),
    as.character(n)
  )
})


test_that("can run saved app", {
  x <- 10
  ui <- fluidPage(
    textOutput("x")
  )
  server <- function(input, output, session) {
    output$x <- renderText(x)
  }

  path <- app_save(shinyApp(ui, server))

  app <- AppDriver$new(path)

  expect_equal(app$get_value(output = "x"), as.character(x))
})



test_that("can get ui and server from app", {
  ui <- fluidPage("Hi!")
  server <- function(input, output, session) {
    "Hello there"
  }

  data <- app_data(shinyApp(ui, server))
  expect_equal(data$ui, ui)
  expect_equal(data$server, server)
})

test_that("can extract globals from server", {
  x <- 10
  server <- function(input, output, session) {
    output$foo <- renderText(input$x + x)
  }

  globals <- app_server_globals(server)
  expect_equal(globals$globals, list(x = 10))
  expect_equal(globals$packages, "shiny")
})
