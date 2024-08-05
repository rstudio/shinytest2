# Related https://github.com/rstudio/shinytest2/issues/388
# Reprex altered from https://stackoverflow.com/questions/78517385/testing-async-extendedtask-with-shinytest2
# 2 issues from reprex:
# * $click() did not work as task button preprocessor wasn't registered
# * Asserting the value of the resolved async output needs to happen after the output value updates due to timeing issues
test_that("bslib task button", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("bslib")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")


  library(shiny)
  library(bslib)
  library(promises)
  library(later)

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
        later::later(function() {
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

  shiny_app <- shinyApp(ui = ui, server = server)

  app <- AppDriver$new(shiny_app)
  app$view()

  app$click("run_normal")
  expect_equal(app$get_value(export = "normal"), 10)

  # Get current value
  cur_async_value <- app$get_value(export = "async")
  app$click("run_async")
  # Wait until expected output changes
  new_async_value <- app$wait_for_value(export = "async", ignore = list(cur_async_value))
  # Assert new value
  expect_equal(new_async_value, 10)
})
