library(shiny)

ui <- fluidPage(
  textInput("val", "UI Output", "<code>Insert HTML here</code>"),
  uiOutput("html"),
  verbatimTextOutput("text")
)
server <- function(input, output, session) {
  output$html <- renderText({
    shiny::req(input$val)
    shiny::HTML(input$val)
  })
  output$text <- renderText({
    shiny::req(input$val)
    input$val
  })
}
shiny_app <- shinyApp(ui, server)


test_that("basic text and dom outputs are expected", {
  app <- AppDriver$new(shiny_app, variant = NULL)

  app$set_inputs(val = "<div id='custom'><p>My Custom Output</p></div>")

  app$expect_text("#text")
  app$expect_text("#custom")

  app$expect_html("#custom", outer_html = TRUE)
  app$expect_html("#custom", outer_html = FALSE)
})

test_that("basic text and dom outputs are captured", {
  app <- AppDriver$new(shiny_app, variant = NULL)

  app$set_inputs(val = "<div id='custom'><p>My Custom Output</p></div>")

  expect_equal(
    app$get_text("#text"),
    "<div id='custom'><p>My Custom Output</p></div>"
  )
  expect_equal(
    app$get_text("#custom"),
    "My Custom Output"
  )

  expect_equal(
    app$get_html("#custom", outer_html = TRUE),
    "<div id=\"custom\"><p>My Custom Output</p></div>"
  )
  expect_equal(
    app$get_html("#custom", outer_html = FALSE),
    "<p>My Custom Output</p>"
  )
})
