fluidPage(
  textInput("name", "What is your name?"),
  actionButton("greet", "Greet"),
  textOutput("greeting"),
  tags$br(),
  # consistenly sized box and consistent across OS
  tags$div(
    id = "custom_div",
    style = "width: 100px;height: 100px;"
  )
)
