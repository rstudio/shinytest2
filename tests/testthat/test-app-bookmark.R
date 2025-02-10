library(shiny)

ui <- function(request) {
  fluidPage(
    textInput("txt", "Enter text"),
    checkboxInput("caps", "Capitalize"),
    verbatimTextOutput("out"),
    bookmarkButton()
  )
}
server <- function(input, output, session) {
  output$out <- renderText({
    if (input$caps) {
      toupper(input$txt)
    } else {
      input$txt
    }
  })
}

shiny_app <- shinyApp(ui, server, enableBookmarking = "url")

test_that("Bookmark works", {
  # Start local app in the background in test mode
  bg_app <- AppDriver$new(shiny_app)

  # Capture the background app's URL and add appropriate query parameters
  bookmark_url <- paste0(bg_app$get_url(), "?_inputs_&txt=%22abcd%22&caps=true")
  # Open the bookmark URL in a new AppDriver object
  app <- AppDriver$new(bookmark_url)

  # Run your tests on the bookmarked `app`
  app$expect_values()
  ## File: _snaps/bookmark/001.json
  # {
  #   "input": {
  #     "._bookmark_": 0,
  #     "caps": true,
  #     "txt": "abcd"
  #   },
  #   "output": {
  #     "out": "ABCD"
  #   },
  #   "export": {
  #
  #   }
  # }
})
