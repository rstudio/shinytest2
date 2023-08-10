library(shiny)

ui <- fluidPage(
  "Upload multiple files:",
  fileInput("files", label = NULL, multiple = TRUE),
  verbatimTextOutput("file_out"),
)

server <- function(input, output) {

  # Display `file` output
  output$file_out <- renderPrint({
    if (is.null(input$files))
      return(NULL)
    df <- input$files
    df$datapath <- paste0("<tempdir>/", basename(df$datapath))
    df
  })
}

shinyApp(ui, server)
