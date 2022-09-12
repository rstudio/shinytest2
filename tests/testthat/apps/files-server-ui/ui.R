library(shiny)

ui <- fluidPage(
  titlePanel("Convert kilograms to grams"),
  p("From", a(href = "https://github.com/rstudio/shinytest2/issues/244", "Issue #244")),
  numericInput("kg", "Weight (in kg)", value = 0),
  textOutput("g")
)
