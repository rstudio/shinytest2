library(shiny)

ui <- fluidPage(
  h1("Click the button and the app dies and returns `42`"),
  actionButton("button", "What is the meaning of life?")
)
server <- function(input, output, session) {
  observeEvent(input$button, {
    shiny::stopApp(42)
  })
}

shiny_app <- shinyApp(ui, server)


test_that("App returns value from script", {
  app <- AppDriver$new(shiny_app)
  app$click("button")
  app$wait_for_idle() # Wait for the app to stop

  meaning_of_life <- app$stop()
  expect_equal(meaning_of_life, 42)
})

test_that("App returns value from script", {
  app <- AppDriver$new(shiny_app)
  # app$click("button")

  normal_return <- app$stop()
  expect_equal(normal_return, NULL)
})
