library(shiny)

ui <- fluidPage(
  downloadButton("download_button_txt", "Download Button - Text"),
  downloadButton("download_link_txt", "Download Link - Text"),
  downloadButton("download_button_csv", "Download Button - CSV"),
  downloadButton("download_link_csv", "Download Link - CSV"),
  downloadButton("download_button_binary", "Download Button - Binary"),
  downloadButton("download_link_binary", "Download Link - Binary"),
)

server <- function(input, output) {

  output$download_button_txt <- downloadHandler(
    filename = function() {
      "download-button.txt"
    },
    content = function(file) {
      cat("Download content!\n", file = file)
    }
  )
  output$download_link_txt <- downloadHandler(
    filename = function() {
      "download-link.txt"
    },
    content = function(file) {
      cat("Download content!\n", file = file)
    }
  )

  output$download_button_csv <- downloadHandler(
    filename = function() {
      "download-button.csv"
    },
    content = function(file) {
      write.csv(head(cars, 10), file)
    }
  )
  output$download_link_csv <- downloadHandler(
    filename = function() {
      "download-link.csv"
    },
    content = function(file) {
      write.csv(tail(cars, 10), file)
    }
  )

  output$download_button_binary <- downloadHandler(
    filename = function() {
      "bear.png"
    },
    content = function(file) {
      file.copy("bear.png", file)
    }
  )
  output$download_link_binary <- downloadHandler(
    filename = function() {
      "bear.png"
    },
    content = function(file) {
      file.copy("bear.png", file)
    }
  )
}

shinyApp(ui, server)
