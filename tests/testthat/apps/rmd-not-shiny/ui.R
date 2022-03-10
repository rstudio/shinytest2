fluidPage(
  title = "Download a PDF report",
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput("x", "Build a regression model of mpg against:",
                  choices = names(mtcars)[-1]),
      radioButtons("format", "Document format", c("PDF", "HTML", "Word"),
                   inline = TRUE),
      downloadButton("download_report")
    ),
    mainPanel(
      plotOutput("reg_plot"),
      verbatimTextOutput("format_type")
    )
  )
)
