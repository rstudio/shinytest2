expkg_internal_value <- "expkg internal value"

#' Exported docs
#' @export
expkg_exported_value <- "expkg exported value"

#' Exported app
#' @export
expkg_shiny_app <- function() {
  library(shiny)
  library(expkg)

  shinyApp(
    ui = fluidPage(
      tags$h1("Local Package Example"),
      tags$p("If this app loads, it is a success!"),
      actionButton("btn", "Click Me"),
      verbatimTextOutput("btn_click_count"),
      verbatimTextOutput("exported_value")
    ),
    server = function(input, output) {
      output$btn_click_count <- renderText({
        input$btn
      })
      output$exported_value <- renderText({
        expkg_exported_value
      })
    }
  )
}

expkg_run_shiny_app <- function() {
  library(expkg)
  shiny::runApp(expkg_shiny_app())
}

expkg_expect_shiny_app <- function(app) {
  testthat::expect_r6_class(app, "AppDriver")

  app$wait_for_idle()

  expect_equal(
    app$get_value(output = "exported_value"),
    "expkg exported value"
  )

  expect_equal(
    app$get_value(output = "btn_click_count"),
    "0"
  )

  app$click("btn")

  expect_equal(
    app$get_value(output = "btn_click_count"),
    "1"
  )
}
