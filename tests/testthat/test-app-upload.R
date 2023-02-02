library(shiny)

ui <- fluidPage(
  "Upload a file:",
  fileInput("file", label = NULL),
  verbatimTextOutput("file_out"),
)

server <- function(input, output) {

  # Display `file` output
  output$file_out <- renderPrint({
    if (is.null(input$file))
      return(NULL)
    df <- input$file
    df$datapath <- paste0("<tempdir>/", basename(df$datapath))
    df
  })
}

shiny_app <- shinyApp(ui, server)

test_that("Make sure upload can use a local file", {
  app <- AppDriver$new(shiny_app)

  app$upload_file(file = test_path("app-files/cars.csv"))
  app$expect_values()

  expect_error(
    app$upload_file(file = "missing_file.csv"),
    "Error finding upload file at path", fixed = TRUE
  )
})
