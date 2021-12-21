library(shiny)

ui <- fluidPage(
  "For testing logging purposes only",
  verbatimTextOutput("time"),
  tags$script("console.log('Log msg')"),
  tags$script("setTimeout(function() { throw 'Exception msg' }, 2)")
)
server <- function(input, output, session) {
  output$time <- renderText({
    invalidateLater(3 * 1000)
    Sys.time()
  })
}

cat("Cat msg!\n")

message("Message msg!")

shinyApp(ui, server)
