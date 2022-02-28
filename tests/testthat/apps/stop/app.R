library(shiny)

ui <- fluidPage(
  h1("Click the button and the app dies and returns `42`"),
  actionButton("button", "What is the meaning of life?")
)
server <- function(input, output, session) {
  observeEvent(input$button, {
    shiny::stopApp(42)
  })
}

shinyApp(ui, server)
