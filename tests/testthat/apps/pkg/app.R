
testthat::expect_error(expkg_internal_value)
testthat::expect_error(expkg_exported_value)

library(expkg)

testthat::expect_error(expkg_internal_value)
testthat::expect_equal(expkg_exported_value, "expkg exported value")

testthat::expect_equal(expkg:::expkg_internal_value, "expkg internal value")

app <- shinyApp(
  ui = fluidPage(
    tags$h1("Local Package Example"),
    tags$p("If this app loads, it is a success!"),
    actionButton("btn", "Click Me"),
    verbatimTextOutput("btn_click_count")
  ), 
  server = function(input, output) {
    output$btn_click_count <- renderText({
      input$btn
    })
  }
)
