library(shiny)

ui <- fluidPage(
  textInput("name", "What is your name?"),
  actionButton("greet", "Greet"),
  textOutput("greeting"),
  tags$br(),
  # consistely sized box and consistent across OS
  tags$div(
    id = "custom_div",
    style = "width: 100px;height: 100px;"
  )
)
server <- function(input, output, session) {
  output$greeting <- renderText({
    shiny::req(input$greet)
    paste0("Hello ", shiny::isolate(input$name), "!")
  })
}

shinyApp(ui, server)
