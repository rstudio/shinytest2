library(shiny)

ui <- fluidPage(
  textInput("val", "UI Output", "<code>Insert HTML here</code>"),
  uiOutput("html"),
  verbatimTextOutput("text")
)
server <- function(input, output, session) {
  output$html <- renderText({
    req(input$val)
    HTML(input$val)
  })
  output$text <- renderText({
    req(input$val)
    input$val
  })
}
shinyApp(ui, server)
