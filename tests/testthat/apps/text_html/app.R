library(shiny)

ui <- fluidPage(
  textInput("val", "UI Output", "<code>Insert HTML here</code>"),
  uiOutput("html"),
  verbatimTextOutput("text")
)
server <- function(input, output, session) {
  output$html <- renderText({
    shiny::req(input$val)
    shiny::HTML(input$val)
  })
  output$text <- renderText({
    shiny::req(input$val)
    input$val
  })
}
shinyApp(ui, server)
