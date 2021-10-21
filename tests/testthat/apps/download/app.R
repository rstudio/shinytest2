ui <- fluidPage(
  downloadButton("download_button", "Download Button"),
  downloadButton("download_link", "Download Link")
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
}

shinyApp(ui, server)
