library(shiny)
library(ggplot2)

ui <- fluidPage(
  numericInput("n", "Number of rows", 10, 1, nrow(cars)),
  plotOutput("plot")
)
server <- function(input, output) {
  dt <- reactive({
    head(cars, input$n)
  })
  plot_obj <- reactive({
    ggplot::ggplot(dt(), ggplot::aes_string("speed", "dist")) + ggplot::geom_point()
  })

  output$plot <- renderPlot({
    plot_obj()
  })

  exportTestValues(
    dt = dt(),
    plot_obj = plot_obj()
  )
}

shinyApp(ui = ui, server = server)
