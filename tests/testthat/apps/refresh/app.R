ui <- fluidPage(
  verbatimTextOutput("time")
)
server <- function(input, output) {
  output$time <- renderText({
    as.numeric(Sys.time())
  })
}

shinyApp(ui, server)
