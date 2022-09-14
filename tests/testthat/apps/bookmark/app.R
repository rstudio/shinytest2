library(shiny)

ui <- function(request) {
  fluidPage(
    textInput("txt", "Enter text"),
    checkboxInput("caps", "Capitalize"),
    verbatimTextOutput("out"),
    bookmarkButton()
  )
}
server <- function(input, output, session) {
  output$out <- renderText({
    if (input$caps) {
      toupper(input$txt)
    } else {
      input$txt
    }
  })
}

shinyApp(ui, server, enableBookmarking = "url")
