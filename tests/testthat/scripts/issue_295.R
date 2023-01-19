library(shiny)

my_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("name"), "name", "test"),
    textOutput(ns("greet"))
  )
}

my_mod_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$greet <- renderText(input$name)
  })
}

make_app2 <- function(mod_ui, mod_server_gen) {
  force(mod_server_gen)
  shiny::shinyApp(
    shiny::fluidPage(mod_ui),
    function(input, output, session) mod_server_gen()
  )
}

app <- make_app2(
  my_mod_ui("test"),
  function() {
    my_mod_server("test")
  }
)

driver <- shinytest2::AppDriver$new(app)

# Keep here to print to `stdout`
print(driver$get_logs())
