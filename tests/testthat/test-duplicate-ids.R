skip_on_cran()
require("shiny", quietly = TRUE, character.only = TRUE)

test_that("Duplicate input ids are found", {

  shiny_app <- shinyApp(
    ui = fluidPage(
      # Duplicate input IDs; Warning!
      textInput("text", "Text 1"),
      textInput("text", "Text 2")
    ),
    server = function(input, output) {
      # empty
    }
  )

  expect_warning(
    app <- AppDriver$new(shiny_app, check_names = TRUE),
    "text"
  )

  expect_failure(
    app$expect_unique_names(),
    "text"
  )

})

test_that("Duplicate input/output ids are found", {

  shiny_app <- shinyApp(
    ui = fluidPage(
      # Duplicate input/output ID; Warning!
      selectInput("select", "Selector", c("A", "B")),
      verbatimTextOutput("select")
    ),
    server = function(input, output) {
      # empty
    }
  )

  expect_warning(
    app <- AppDriver$new(shiny_app, check_names = TRUE),
    "select"
  )

  expect_failure(
    app$expect_unique_names(),
    "select"
  )

})



test_that("Duplicate output ids are found", {

  shiny_app <- shinyApp(
    ui = fluidPage(
      div(id = "custom", class = "shiny-bound-output", "Div 1 content"),
      div(id = "custom", class = "shiny-bound-output", "Div 2 content")
    ),
    server = function(input, output) {
      # empty
    }
  )

  expect_warning(
    app <- AppDriver$new(shiny_app, check_names = TRUE),
    "custom"
  )

  expect_failure(
    app$expect_unique_names(),
    "custom"
  )
})


test_that("Duplicate input ids are found", {
  shiny_app <- shinyApp(
    ui = fluidPage(
      # Duplicate output IDs causes failure to load application
      htmlOutput("html"),
      textOutput("html")
    ),

    server = function(input, output) {
      # empty
    }
  )

  expect_error(
    AppDriver$new(shiny_app, load_timeout = 1000),
    "Shiny app did not become stable"
  )
})
