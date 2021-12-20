library(shiny)

ui <- fluidPage(
  "For testing logging purposes only",
  tags$script("console.log('Log msg')"),
  tags$script("setTimeout(function() { throw 'Exception msg' }, 2)")
)
server <- function(input, output, session) { }

cat("Cat msg!\n")

message("Message msg!")

shinyApp(ui, server)
