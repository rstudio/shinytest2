library(shiny)

# App that uses `renderImage()`

ui <- fluidPage(
  h3("A bear"),
  actionButton("rawr", "Rawr!"),
  tags$br(),
  imageOutput("img", width = 500, height = 500),

  tags$br(),
  h3("A red box"),
  div(
    id = "red",
    style = "background-color: red; width: 100px; height: 100px;"
  ),
  h3("A green box"),
  div(
    id = "green",
    style = "background-color: darkgreen; width: 100px; height: 100px;"
  ),
)

server <- function(input, output, session) {
  output$img <- renderImage(
    {
      shiny::req(input$rawr)

      list(src = test_path("app-files/bear.png"))
    },
    deleteFile = FALSE
  )
}

shiny_app <- shinyApp(ui, server)


test_that("images are captured via expect_values", {
  app <- AppDriver$new(
    shiny_app,
    variant = platform_variant(r_version = FALSE)
    # name = "values-image"
  )

  app$click("rawr")
  app$expect_values()

  # Add something that will always produce a new image
  app$run_js(
    '
    $("body").append("<div>" + new Date() + "</div>")
  '
  )

  # Since it is zoomed in on the image, the text will be NOT be displayed
  app$expect_values(output = "img")

  # # Uncomment to test always new screenshot via `$expect_values()`
  # app$expect_values()
})

# TODO-future; Perform these tests via mock to assert proper screenshot args are captured.
test_that("Values screenshot args are used", {
  app <- AppDriver$new(
    shiny_app,
    variant = NULL,
    name = "sa-values",
    expect_values_screenshot_args = list(selector = "#green")
  )

  # should take a picture of `#green`, not app
  app$expect_values()
})

test_that("User screenshot args are used instead of auto defined screenshot args", {
  app <- AppDriver$new(
    shiny_app,
    variant = NULL,
    name = "sa-user",
    expect_values_screenshot_args = list(selector = "#red")
  )

  # should take a picture of `#green`, not `#red` or app
  app$expect_values(screenshot_args = list(selector = "#green"))
})

# TODO-future; Perform these tests via mock to assert proper screenshot args are captured.
test_that("No screenshot is taken", {
  app <- AppDriver$new(
    shiny_app,
    variant = NULL,
    name = "no-pic1",
    expect_values_screenshot_args = FALSE
  )

  # No picture
  app$expect_values()

  app2 <- AppDriver$new(
    shiny_app,
    variant = NULL,
    name = "no-pic2"
    # screenshot_args = rlang::missing_arg()
  )
  # No picture
  app2$expect_values(screenshot_args = FALSE)
})

test_that("screenshot can be expected", {
  app <- AppDriver$new(
    shiny_app,
    variant = NULL,
    name = "screen1",
    screenshot_args = list(selector = "#green")
  )

  # This directly calls `app$get_screenshot()`, no need for extra testing
  # Should take a picture of `#green`, not app
  app$expect_screenshot()
})
test_that("screenshot can be expected", {
  app <- AppDriver$new(
    shiny_app,
    variant = NULL,
    name = "screen2"
  )

  # This directly calls `app$get_screenshot()`, no need for extra testing
  # Should take a picture of `#green`, not app
  app$expect_screenshot(
    screenshot_args = list(selector = "#green")
  )
})
