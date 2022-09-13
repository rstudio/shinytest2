library(shiny)

# From: https://github.com/rstudio/shinytest2/issues/232

ui <- fluidPage(
  selectizeInput(
    "select", "Select",
    choices = NULL,
    options = list(maxOptions = 5)
  )
)
server <- function(input, output, session) {
  updateSelectizeInput(
    inputId = "select",
    choices = 1:10,
    server = TRUE
  )
}

shinyApp(ui = ui, server = server)
