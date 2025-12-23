skip_if_not_installed("ggplot2")

library(shiny)
library(ggplot2)

ui <- fluidPage(
  numericInput("n", "Number of rows", 10, 1, nrow(cars)),
  plotOutput("plot")
)
server <- function(input, output) {
  dt <- reactive({
    head(cars, input$n)
  })
  plot_obj <- reactive({
    ggplot2::ggplot(dt(), ggplot2::aes_string("speed", "dist")) +
      ggplot2::geom_point()
  })

  output$plot <- renderPlot({
    plot_obj()
  })

  exportTestValues(
    dt = dt(),
    plot_obj = plot_obj()
  )
}

shiny_app <- shinyApp(ui = ui, server = server)


test_that("`export`ed `plot_obj` is updated by `n`", {
  skip_if_not_installed("vdiffr")

  app <- AppDriver$new(shiny_app)

  expect_n_and_plot <- function(n) {
    # Verify `n` input equals `n`
    n_val <- app$get_value(input = "n")
    testthat::expect_equal(n_val, n, expected.label = n)
    # Verify `dt()` data is first `n` lines of `cars`
    dt <- app$get_value(export = "dt")
    testthat::expect_equal(
      dt,
      head(cars, n),
      expected.label = paste0("head(cars, ", n, ")")
    )

    # Verify `plot_obj()` data is `dt()`
    plot_obj <- app$get_value(export = "plot_obj")
    testthat::expect_equal(plot_obj$data, dt, info = paste0("n = ", n))
    # Verify `plot_obj()` is consistent
    vdiffr::expect_doppelganger(paste0("cars-points-", n), plot_obj)
  }

  expect_n_and_plot(10)

  app$set_inputs(n = 20)
  expect_n_and_plot(20)
})


test_that("`export`ed `plot_obj` is updated by `n`", {
  skip_if_not_installed("vdiffr")

  app <- AppDriver$new(shiny_app)

  # Verify `dt()` uses first 10 lines of `cars`
  n10 <- app$get_value(input = "n")
  expect_equal(n10, 10)
  # Verify `dt10()` data is first 10 lines of `cars`
  dt10 <- app$get_value(export = "dt")
  expect_equal(dt10, head(cars, n10))

  # Verify `plot_obj()` data is `dt()`
  plot_obj_10 <- app$get_value(export = "plot_obj")
  expect_equal(plot_obj_10$data, dt10)
  # Verify `plot_obj()` is consistent
  vdiffr::expect_doppelganger("cars-points-10-export", plot_obj_10)

  ## Update `n` to 20
  app$set_inputs(n = 20)

  # Verify `n` was updated
  n20 <- app$get_value(input = "n")
  expect_equal(n20, 20)
  # Verify `dt()` uses first 20 lines of `cars`
  dt20 <- app$get_value(export = "dt")
  expect_equal(dt20, head(cars, n20))

  # Verify `plot_obj()` data is `dt()`
  plot_obj_20 <- app$get_value(export = "plot_obj")
  expect_equal(plot_obj_20$data, dt20)
  vdiffr::expect_doppelganger("cars-points-20-export", plot_obj_20)
})


test_that("Exported values", {
  shiny_app <-
    shinyApp(
      ui = basicPage(
        h4("Snapshot URL: "),
        uiOutput("url"),
        h4("Current values:"),
        verbatimTextOutput("values"),
        actionButton("inc", "Increment x")
      ),

      server = function(input, output, session) {
        vals <- reactiveValues(x = 1)
        y <- reactive({
          vals$x + 1
        })

        observeEvent(input$inc, {
          vals$x <<- vals$x + 1
        })

        exportTestValues(
          x = vals$x,
          y = y()
        )

        output$url <- renderUI({
          url <- session$getTestSnapshotUrl(format = "json")
          a(href = url, url)
        })

        output$values <- renderText({
          paste0("vals$x: ", vals$x, "\ny: ", y())
        })
      }
    )

  app <- AppDriver$new(shiny_app)

  x <- app$get_values()
  expect_identical(x$export$x, 1)
  expect_identical(x$export$y, 2)

  app$set_inputs(inc = "click")
  app$set_inputs(inc = "click")

  x <- app$get_values()
  expect_identical(x$export$x, 3)
  expect_identical(x$export$y, 4)
})
