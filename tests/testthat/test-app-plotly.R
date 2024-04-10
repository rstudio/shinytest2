skip_if_not_installed("plotly")

library(shiny)

ui <- fluidPage(
  plotly::plotlyOutput(outputId = "p")
)
server <- function(input, output, session, ...) {
  output$p <- plotly::renderPlotly({
      plotly::plot_ly(x = cars[, 1], y = cars[, 2], type = "scattergl", mode = "markers")
    }) |>
      shiny::snapshotPreprocessOutput(function(p) {
        info <- jsonlite::parse_json(p, simplifyVector = TRUE, simplifyDataFrame = FALSE,
          simplifyMatrix = FALSE)
        info$x$data[[1]][c("x", "y")]
      })
}
shiny_app <- shinyApp(ui, server)


test_that("plotly webgl works", {
  # TODO-future; Good candidate for fuzzy picture matching

  app <- AppDriver$new(shiny_app)

  app$wait_for_value(output = "p", ignore = list(NULL))

  app$expect_values(output = "p", screenshot_args = FALSE)
})
