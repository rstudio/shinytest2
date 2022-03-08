library(shiny)

ui <- fluidPage(
  "For testing logging purposes only",
  verbatimTextOutput("time"),
  # Print all known types
  tags$script("console.log('Nullish', null, undefined)"),
  tags$script("console.log('Boolean', false, true)"),
  tags$script("console.log('Character', 'abc')"),
  tags$script("console.log('Number', 123)"),
  tags$script("console.log('BigInt', 10n)"),
  tags$script("console.log('Object', {'a': 'b'})"),
  tags$script("console.log('Math', Math)"),
  tags$script("console.log('Symbol', Symbol('abc'))"),
  tags$script("console.log('Array', [1,2, 3])"),
  tags$script("console.log('Function', Date)"),
  # Capture throw
  tags$script("setTimeout(function() { throw 'Exception msg' }, 2)"),
  # Capture exception
  tags$script("setTimeout(function() { window.test_method(); }, 4)"),
)
server <- function(input, output, session) {
  cat("Cat msg!\n")

  message("Message msg!")

  output$time <- renderText({
    shiny::invalidateLater(3 * 1000)
    Sys.time()
  })
}

shinyApp(ui, server)
