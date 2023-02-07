library(shiny)


test_that("images are captured via expect_values", {
  img_height <- 501
  img_width <- 502

  bear_path <- normalizePath(test_path("app-files/bear.png"))
  ui <- fluidPage(
    imageOutput("img", width = img_width, height = img_height),
  )
  server <- function(input, output, session) {
    output$img <- renderImage({
      list(src = bear_path)
    }, deleteFile = FALSE)
  }
  shiny_app <- shinyApp(ui, server)

  expect_screenshot <- function(app, selector, size) {
    tmpfile <- withr::local_tempfile(fileext=".png")
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
      imgS1 <- expect_screenshot(app, rlang::missing_arg(), c(
        max(c(img_height, window_size$height)),
        # Image width + 15 px margin left from container
        max(c(img_width + 15, window_size$width))
      ))

      # Browser Viewport
      imgV1 <- expect_screenshot(app, "viewport", c(window_size$height, window_size$width))

      if (window_size$scroll) {
        # Introduce a scroll
        app$run_js("window.scroll(5, 20)")

        # Scroll value (default)
        imgS2 <- expect_screenshot(app, "scroll", c(
          max(c(img_height, window_size$height)),
          # Image width + 15 px margin left from container
          max(c(img_width + 15, window_size$width))
        ))

        # Browser Viewport
        imgV2 <- expect_screenshot(app, "viewport", c(window_size$height, window_size$width))
      }

      if (window_size$scroll) {
        # Make sure the images are not shifted
        expect_true(imgS1[86, 203, 1] == 1)
        expect_true(imgS1[106, 208, 1] < 1)
        expect_true(imgS2[86, 203, 1] == 1)
        expect_true(imgS2[106, 208, 1] < 1)
        expect_equal(imgS1[106, 208, 1], imgS2[106, 208, 1])

        # Make sure imgV1 is the same as imgS1 and imgS2
        expect_true(imgV1[86, 203, 1] == 1)
        expect_true(imgV1[106, 208, 1] < 1)
        expect_equal(imgV1[106, 208, 1], imgS1[106, 208, 1])
        expect_equal(imgV1[106, 208, 1], imgS2[106, 208, 1])

        # Make sure imgV2 has new (shifted) pixel value
        expect_true(imgV2[86, 203, 1] < 1)
        expect_true(imgV2[86, 203, 1] != imgV2[106, 208, 1])
        # Make sure the imgV has the same shifted value as the original images
        expect_equal(imgV2[86, 203, 1], imgS1[106, 208, 1])
        expect_equal(imgV2[86, 203, 1], imgS2[106, 208, 1])
      }
    })
  }
})

