library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput(outputId = "p")
)

server <- function(input, output, session, ...) {
  output$p <- shiny::snapshotPreprocessOutput(
    renderPlotly({
      plot_ly(x = cars[,1], y = cars[,2], type = "scattergl", mode = "markers")
    }),
    function(p) {
      jsonlite::parse_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE)$x$data[[1]][c("x", "y")]
    }
  )
}

shinyApp(ui, server)
