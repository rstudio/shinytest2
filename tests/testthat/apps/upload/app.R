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

shinyApp(ui, server)
