library(shiny)

ui <- fluidPage(
  h1("Click the button and the app dies and returns `42`"),
  actionButton("button", "What is the meaning of life?")
)
server <- function(input, output, session) {
  observeEvent(input$button, {
    shiny::stopApp(42)
  })
}

shiny_app <- shinyApp(ui, server)


test_that("App returns value from script", {
  app <- AppDriver$new(shiny_app)
  app$click("button")
  app$wait_for_idle() # Wait for the app to stop

  meaning_of_life <- app$stop()
  expect_equal(meaning_of_life, 42)
})

test_that("get_process() can wait for an app that stops itself", {
  # https://github.com/rstudio/shinytest2/issues/319
  done_app <- shinyApp(
    fluidPage(actionButton("done", "Done")),
    function(input, output, session) {
      observeEvent(input$done, {
        assign("foo", "bar", envir = globalenv())
        shiny::stopApp()
      })
    }
  )
  app <- AppDriver$new(function() {
    shiny::runApp(done_app)
    Sys.sleep(1) # Slow cleanup after the app stops
    mget("foo", envir = globalenv())
  })
  app$click("done", wait_ = FALSE)

  app$get_process()$wait(5000)
  expect_false(app$get_process()$is_alive())
  expect_equal(app$stop(), list(foo = "bar"))
})

test_that("get_process() reports if the app is running", {
  # https://github.com/rstudio/shinytest2/issues/320
  app <- AppDriver$new(shiny_app)
  expect_s3_class(app$get_process(), "r_process")
  expect_true(app$get_process()$is_alive())

  app$stop()
  expect_false(app$get_process()$is_alive())
})

test_that("App returns value from script", {
  app <- AppDriver$new(shiny_app)
  # app$click("button")

  normal_return <- app$stop()
  expect_equal(normal_return, NULL)
})
