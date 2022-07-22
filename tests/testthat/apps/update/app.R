library(shiny)

ui <- fluidPage(
  tags$h1("Do not touch slider"),
  actionButton("click", "Click me!"),
  tagAppendAttributes(sliderInput("slider", "Slider", 0, 100, value = 50), class = "disabled"),
  tags$h4("Click count"),
  verbatimTextOutput("clicks"),
  tags$h4("JS Click count"),
  verbatimTextOutput("js_clicks"),
  tags$script(HTML('
    $(function() {
      Shiny.addCustomMessageHandler("count", function(message) {
        Shiny.setInputValue("counter", message, {priority: "event"});
      });
    });
  '))
)

server <- function(input, output, session) {

  observe({
    shiny::updateSliderInput(inputId = "slider", value = input$click)
    session$sendCustomMessage("count", input$click)
  })

  output$clicks <- renderText({
    input$click
  })
  output$js_clicks <- renderText({
    input$counter
  })

}

shinyApp(ui, server)
