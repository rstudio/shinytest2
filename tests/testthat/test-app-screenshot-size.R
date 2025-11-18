library(shiny)


test_that("images are captured via expect_values", {
  skip_on_cran()

  img_height <- 501
  img_width <- 502

  bear_path <- normalizePath(test_path("app-files/bear.png"))
  ui <- fluidPage(
    imageOutput("img", width = img_width, height = img_height),
  )
  server <- function(input, output, session) {
    output$img <- renderImage(
      {
        list(src = bear_path)
      },
      deleteFile = FALSE
    )
  }
  shiny_app <- shinyApp(ui, server)

  expect_screenshot <- function(app, selector, size) {
    tmpfile <- withr::local_tempfile(fileext = ".png")
    app$get_screenshot(tmpfile, selector = selector)
    pngdata <- png::readPNG(tmpfile)
    expect_equal(dim(pngdata)[1:2], size)
    pngdata
  }

  app_window_sizes <- rlang::list2(
    list(height = 400, width = 450, scroll = TRUE), # smaller than content
    list(height = 600, width = 650, scroll = FALSE), # bigger than content
  )

  for (window_size in app_window_sizes) {
    local({
      app <- AppDriver$new(
        shiny_app,
        width = window_size$width,
        height = window_size$height
      )
      withr::defer(app$stop())
      # app$wait_for_idle()

      # Default value
      img_s1 <- expect_screenshot(
        app,
        rlang::missing_arg(),
        c(
          max(c(img_height, window_size$height)),
          # Image width + 15 px margin left from container
          max(c(img_width + 15, window_size$width))
        )
      )

      # Browser Viewport
      img_v1 <- expect_screenshot(
        app,
        "viewport",
        c(window_size$height, window_size$width)
      )

      if (window_size$scroll) {
        # Introduce a scroll
        app$run_js("window.scroll(5, 20)")

        # Scroll value (default)
        img_s2 <- expect_screenshot(
          app,
          "scrollable_area",
          c(
            max(c(img_height, window_size$height)),
            # Image width + 15 px margin left from container
            max(c(img_width + 15, window_size$width))
          )
        )

        # Browser Viewport
        img_v2 <- expect_screenshot(
          app,
          "viewport",
          c(window_size$height, window_size$width)
        )
      }

      if (window_size$scroll) {
        # Make sure the images are not shifted
        expect_true(img_s1[86, 203, 1] == 1)
        expect_true(img_s1[106, 208, 1] < 1)
        expect_true(img_s2[86, 203, 1] == 1)
        expect_true(img_s2[106, 208, 1] < 1)
        expect_equal(img_s1[106, 208, 1], img_s2[106, 208, 1])

        # Make sure img_v1 is the same as img_s1 and img_s2
        expect_true(img_v1[86, 203, 1] == 1)
        expect_true(img_v1[106, 208, 1] < 1)
        expect_equal(img_v1[106, 208, 1], img_s1[106, 208, 1])
        expect_equal(img_v1[106, 208, 1], img_s2[106, 208, 1])

        # Make sure img_v2 has new (shifted) pixel value
        expect_true(img_v2[86, 203, 1] < 1)
        expect_true(img_v2[86, 203, 1] != img_v2[106, 208, 1])
        # Make sure the img_v has the same shifted value as the original images
        expect_equal(img_v2[86, 203, 1], img_s1[106, 208, 1])
        expect_equal(img_v2[86, 203, 1], img_s2[106, 208, 1])
      }
    })
  }
})


test_that("app with no `html` height can get a screenshot", {
  skip_on_cran()
  skip_on_os("windows")

  shiny_app <- shinyApp(
    div(
      style = "height:400px; background:red;",
      tags$head(tags$style("html {height: 0}"))
    ),
    function(...) {
      # No server code
    }
  )

  height <- 750
  width <- 850

  app <- AppDriver$new(shiny_app, height = height, width = width)

  expect_no_screenshot_error <- function(selector, error_msg = NA) {
    tmpfile <- withr::local_tempfile(fileext = ".png")
    withr::local_options(list(warn = 2))
    expect_error(
      app$get_screenshot(tmpfile, selector = selector),
      error_msg
    )
  }
  # No error
  expect_no_screenshot_error(rlang::missing_arg())
  expect_no_screenshot_error("scrollable_area")
  expect_no_screenshot_error("viewport")
  # Produces error
  expect_no_screenshot_error("html", error_msg = "with 0 height")
})
