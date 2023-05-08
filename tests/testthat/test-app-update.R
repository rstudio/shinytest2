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

shiny_app <- shinyApp(ui, server)


test_that("click causes input without binding to update", {
  app <- AppDriver$new(shiny_app, name = "click")

  app$click("click")
  app$click("click")
  app$click("click")
  app$click("click")
  app$expect_values()
})

test_that("Can update the input without biding individually", {
  app <- AppDriver$new(shiny_app, name = "no-binding")

  app$set_inputs(counter = 1, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(counter = 2, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(counter = 3, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(counter = 4, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_values()
})
