expkg_internal_value <- "expkg internal value"

#' Exported docs
#' @export
expkg_exported_value <- "expkg exported value"

#' Exported app
#' @export
expkg_shiny_app <- function() {
  library(shiny)

  shinyApp(
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
}
