library(shiny)

txt_val <- get0("shinytest2_profile_value", ifnotfound = "Not found! 😔😭")

ui <- fluidPage(
  "This app must be run in a clean R session that is started in the same directory as the app.R file.",
  textOutput("txt"),
)
server <- function(input, output, session) {
  output$txt <- renderText(txt_val)
}

shinyApp(ui, server)
