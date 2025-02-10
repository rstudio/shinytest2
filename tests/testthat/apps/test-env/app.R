library(shiny)

testthat::expect_true(
  inherits(try(AppDriver, silent = TRUE), "try-error")
)

# Test for internal value
# This is defined within shinytest2's ./R/app-driver-start.R
# and will only allow the app to run if the app has access to shinytest2's internal functions
# or the "local package" values.
value <- shinytest2:::internal_shinytest2_value

ui <- fluidPage(
  tags$h1("Internal test value:"),
  verbatimTextOutput("value", placeholder = TRUE),
)
server <- function(input, output, session) {
  output$value <- renderText({
    value
  })
}

shinyApp(ui, server)
