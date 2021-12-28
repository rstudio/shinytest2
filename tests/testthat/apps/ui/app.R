library(shiny)

ui <- fluidPage(
  h1("Dynamic UI"),
  uiOutput("first")
)
server <- function(input, output, session) {

  # This app appears in three stages.
  # If the logic was to "wait for idle", it would return after the first stage is init'ed
  # If the logic is to "wait for stable", it would return after sitting idle for a set amount of time. Ex 1500ms


  timeout <- n # Found in `./R/n.R` # nolint

  first_flag <- FALSE
  second_flag <- FALSE
  third_flag <- FALSE

  output$first <- renderUI({
    if (!first_flag) {
      first_flag <<- TRUE
      shiny::invalidateLater(timeout)
      return(
        h3("(waiting on first)")
      )
    }

    list(
      h3("first"),
      sliderInput("slider1", "Slider1", 0, 10, 1),
      uiOutput("second")
    )
  })
  output$second <- renderUI({
    if (!second_flag) {
      second_flag <<- TRUE
      shiny::invalidateLater(timeout)
      return(
        h3("(waiting on second)")
      )
    }

    list(
      h3("second"),
      sliderInput("slider2", "Slider2", 0, 10, 2),
      uiOutput("third")
    )
  })
  output$third <- renderUI({
    if (!third_flag) {
      third_flag <<- TRUE
      shiny::invalidateLater(timeout)
      return(
        h3("(waiting on third)")
      )
    }

    list(
      h3("third"),
      sliderInput("slider3", "Slider3", 0, 10, 3),
      uiOutput("verbatim_txt")
    )
  })
  output$verbatim_txt <- renderUI({
    shiny::verbatimTextOutput("txt")
  })
  output$txt <- renderText({
    paste(input$slider1, input$slider2, input$slider3, sep = " ")
  })
}

shinyApp(ui, server)
