library(shiny)


function(input, output, session) {
  output$greeting <- renderText({
    shiny::req(input$greet)
    paste0("Hello ", shiny::isolate(input$name), "!")
  })
}
