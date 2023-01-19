library(shiny)

mod_ui <- function(id) {
  ns <- NS(id)
  textOutput(ns("foo"))
}

mod_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$foo <- renderText("bar")
    }
  )
}

app <- shinyApp(
  mod_ui("test"),
  function(input, output, session) {
    mod_server("test")
  }
)

driver <- shinytest2::AppDriver$new(app)

# Keep here to print to `stdout`
print(driver$get_logs())
