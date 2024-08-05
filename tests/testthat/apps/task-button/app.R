library(shiny)
yoink <- function(pkg, fn_name) {
  getFromNamespace(fn_name, pkg)
}
promise <- yoink("promises", "promise")
later <- yoink("later", "later")
input_task_button <- yoink("bslib", "input_task_button")
bind_task_button <- yoink("bslib", "bind_task_button")



ui <- fluidPage(
  actionButton("run_normal", "Run normal"),
  textOutput("normal_result"),
  input_task_button("run_async", "Run async"),
  textOutput("async_result")
)

server <- function(input, output, session) {
  returned_value <- reactiveValues()

  slow_function <- function() {
    return(10)
  }

  task <- ExtendedTask$new(function() {
    promise(function(resolve, reject) {
      # Use later to simulate future promise calls
      later(function() {
        resolve(slow_function())
        # Add extra time for extra checks
      }, delay = 0.01)
    })
  }) |> bind_task_button("run_async")


  # Normal
  observeEvent(input$run_normal, {
    returned_value$normal <- slow_function()
  })

  output$normal_result <- renderText({
    req(returned_value$normal)
    returned_value$normal
  })

  # Async
  observeEvent(input$run_async, {
    task$invoke()
  })

  observe({
    returned_value$async <- task$result()
  })

  output$async_result <- renderText({
    req(returned_value$async)
    returned_value$async
  })

  # Export for testing
  exportTestValues(
    async = returned_value$async,
    normal = returned_value$normal
  )
}

shinyApp(ui = ui, server = server)
