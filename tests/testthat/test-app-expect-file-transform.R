require("shiny", quietly = TRUE, character.only = TRUE)

shiny_app <- shinyApp(
  ui = fluidPage(
    downloadButton("download_button_txt", "Download Button - Text"),
    verbatimTextOutput("txt")
  ),
  server = function(input, output) {
    output$download_button_txt <- downloadHandler(
      filename = function() {
        "download-button.txt"
      },
      content = function(file) {
        cat("Download content!\n", file = file)
      }
    )

    output$txt <- renderText({
      "(app loaded)"
    })
  }
)


test_that("transform can be passed to expect_snapshot_file()", {
  app <- AppDriver$new(shiny_app)

  download_called <- 0
  values_called <- 0

  app$expect_download("download_button_txt", transform = function(...) {
    download_called <<- download_called + 1
    list(...)[[1]]
  })
  app$expect_values(transform = function(...) {
    values_called <<- values_called + 1
    list(...)[[1]]
  })

  expect_equal(download_called, 1)
  expect_equal(values_called, 1)
})
