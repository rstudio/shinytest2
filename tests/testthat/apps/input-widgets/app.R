# Combined from: https://github.com/rstudio/shinycoreci-apps/blob/3951d87bd91f27928a4cdf439c113590ce804508/apps/081-widgets-gallery/

widget_ids <- NULL
widget <- function(name, widget_object, title = NULL) {
  # Collect all widget ids
  widget_ids <<- append(widget_ids, name)

  column(4,
    wellPanel(
      if (!is.null(title)) h3(title),
      widget_object,
      hr(),
      p("Current Value:", style = "color:#888888;"),
      verbatimTextOutput(paste0(name, "_out"))
    )
  )
}

widget_gallery <- tabPanel(
  title = strong("{shiny}"),
  value = "shiny",

  h1("{shiny}", span("Gallery", style = "font-weight: 300"), style = "text-align: center;"),
  br(),

  fluidRow(
    column(6, offset = 3,
      p("For each widget below, the Current Value(s) window
        displays the value that the widget provides to shinyServer.
        Notice that the values change as you interact with the widgets.")
    )
  ),


  br(),

  fluidRow(

    widget("action", title = "Action button",
      actionButton("action", label = "Action")
    ),
    widget("checkbox", title = "Single checkbox",
      checkboxInput("checkbox", label = "Choice A", value = FALSE)
    ),
    widget("checkGroup",
      checkboxGroupInput("checkGroup",
          label = h3("Checkbox group"),
          choices = list("Choice 1" = 1, "Choice 2" = 2,
                         "Choice 3" = 3),
          selected = 1)
    ),
  ),

  fluidRow(
    widget("date", dateInput("date", label = h3("Date input"), value = "2014-01-01")),
    widget("dates", dateRangeInput("dates", label = h3("Date range"), start = "2014-01-01", end = "2014-02-20")),
    widget("file", fileInput("file", label = h3("File input"))),
  ),

  fluidRow(
    widget("num", numericInput("num", label = h3("Numeric input"), value = 1)),
    widget(
      "radio",
      radioButtons(
        "radio", label = h3("Radio buttons"),
        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
        selected = 1
      )
    ),
    widget(
      "select",
      selectInput(
        "select",
        label = h3("Select box"),
        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
        selected = 1
      )
    ),
  ),


  fluidRow(
    widget("slider1", sliderInput("slider1", label = h3("Slider"), min = 0, max = 100, value = 50)),
    widget("slider2", sliderInput("slider2", label = h3("Slider range"), min = 0, max = 100, value = c(25, 75))),
    widget("text", textInput("text", label = h3("Text input"), value = "Enter text...")),
  )
)

shiny_widgets <- tabPanel(
  title = strong("{shinyWidgets}"),
  value = "shinyWidgets",

  h1("{shinyWidgets}", span("Gallery", style = "font-weight: 300"), style = "text-align: center;"),
  br(),

  fluidRow(
    column(6, offset = 3,
      p("For each widget below, the Current Value(s) window
        displays the value that the widget provides to shinyServer.
        Notice that the values change as you interact with the widgets.")
    )
  ),


  br(),

  fluidRow(
    widget("bsSwitch", shinyWidgets::switchInput(inputId = "bsSwitch"), title = "Bootstrap switch"),
    widget("matSwitch", shinyWidgets::materialSwitch(inputId = "matSwitch", label = "Primary switch", status = "danger"), title = "Material switch"),
    widget("prettyCheckbox", shinyWidgets::prettyCheckbox(inputId = "prettyCheckbox", label = "Check me!", status = "success", outline = TRUE), title = "Pretty checkbox")
  ),
  fluidRow(
    widget(
      "sliderText",
      shinyWidgets::sliderTextInput(
        inputId = "sliderText",
        label = "Your choice:",
        grid = TRUE,
        force_edges = TRUE,
        choices = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
      )
    ),
    widget("knob", title = "jQuery knob", shinyWidgets::knobInput(
      inputId = "knob",
      label = "jQuery knob example:",
      value = 0,
      min = -100,
      displayPrevious = TRUE,
      lineCap = "round",
      fgColor = "#428BCA",
      inputColor = "#428BCA"
    )),
    widget("picker", shinyWidgets::pickerInput(
      inputId = "picker",
      label = "Select/deselect all + format selected",
      choices = LETTERS,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    ))
  ),
  fluidRow(
    widget("checkboxGroupButtons", shinyWidgets::checkboxGroupButtons(
      inputId = "checkboxGroupButtons",
      label = "Make a choice :",
      choices = c("Choice A", "Choice B", " Choice C", "Choice D"),
      justified = TRUE, status = "primary"
    )),
    widget("search", shinyWidgets::searchInput(
      inputId = "search",
      label = "Enter your search (hit `Enter` when ready):",
      placeholder = "This is a placeholder",
      width = "100%"
    ))
  )
)

ui <- fluidPage(

  tags$head(tags$style(HTML("
    .shiny-text-output {
      background-color:#fff;
    }
    /* .row .well { */
    /*  border-radius: 0;*/
    /*}*/
  "))),

  tabsetPanel(
    id = "tabset",
    widget_gallery,
    shiny_widgets
  )
)



server <- function(input, output) {

  # Display all widget outputs (except for `file`)
  lapply(widget_ids, function(widget_id) {
    if (widget_id %in% "file") return()

    output[[paste0(widget_id, "_out")]] <- renderPrint({
      input[[widget_id]]
    })
  })

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
