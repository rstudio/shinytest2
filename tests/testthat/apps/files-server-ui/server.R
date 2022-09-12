library(shiny)

server <- function(input, output, session) {
  output$g <- renderText(paste0("Weight (in g): ", input$kg * 1000))
}
