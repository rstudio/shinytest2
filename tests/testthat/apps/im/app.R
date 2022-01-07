library(shiny)

# App that uses `renderImage()`

ui <- fluidPage(
  h3("A bear"),
  actionButton("rawr", "Rawr!"),
  tags$br(),
  imageOutput("img", width = 500, height = 500),

  tags$br(),
  h3("A red box"),
  div(id = "red", style = "background-color: red; width: 100px; height: 100px;"),
  h3("A green box"),
  div(id = "green", style = "background-color: darkgreen; width: 100px; height: 100px;"),
)

server <- function(input, output, session) {
  output$img <- renderImage({
    shiny::req(input$rawr)

    list(src = "bear.png")
  }, deleteFile = FALSE)

}

shinyApp(ui, server)
