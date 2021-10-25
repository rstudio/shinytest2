ui <- fluidPage(
  downloadButton("download_button", "Download Button"),
  downloadButton("download_link", "Download Link"),
  verbatimTextOutput("add")
)

server <- function(input, output) {

  output$download_button <- downloadHandler(
    filename = function() {
      "download-button.csv"
    },
    content = function(file) {
      write.csv(head(cars, 10), file)
    }
  )
  output$download_link <- downloadHandler(
    filename = function() {
      "download-link.csv"
    },
    content = function(file) {
      write.csv(tail(cars, 10), file)
    }
  )

  # Used to add a `tick` into the app / slow down testing
  output$add <- renderText({
    1 + 1
  })
}

shinyApp(ui, server)
