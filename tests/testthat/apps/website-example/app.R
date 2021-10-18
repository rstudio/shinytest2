library(shiny)

ui <- fluidPage(
  textInput("name", "What is your name?"),
  actionButton("greet", "Greet"),
  textOutput("greeting")
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    shiny::req(input$greet)
    paste0("Hello ", shiny::isolate(input$name), "!")
  })
}
shinyApp(ui, server)
