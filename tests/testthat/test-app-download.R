library(shiny)

bear_img <- fs::path_abs(test_path("app-files/bear.png"))

ui <- fluidPage(
  downloadButton("download_button_txt", "Download Button - Text"),
  downloadButton("download_link_txt", "Download Link - Text"),
  downloadButton("download_button_csv", "Download Button - CSV"),
  downloadButton("download_link_csv", "Download Link - CSV"),
  downloadButton("download_button_binary", "Download Button - Binary"),
  downloadButton("download_link_binary", "Download Link - Binary"),
)

server <- function(input, output) {
  output$download_button_txt <- downloadHandler(
    filename = function() {
      "download-button.txt"
    },
    content = function(file) {
      cat("Download content!\n", file = file)
    }
  )
  output$download_link_txt <- downloadHandler(
    filename = function() {
      "download-link.txt"
    },
    content = function(file) {
      cat("Download content!\n", file = file)
    }
  )

  output$download_button_csv <- downloadHandler(
    filename = function() {
      "download-button.csv"
    },
    content = function(file) {
      write.csv(head(cars, 10), file)
    }
  )
  output$download_link_csv <- downloadHandler(
    filename = function() {
      "download-link.csv"
    },
    content = function(file) {
      write.csv(tail(cars, 10), file)
    }
  )

  output$download_button_binary <- downloadHandler(
    filename = function() {
      "bear.png"
    },
    content = function(file) {
      file.copy(bear_img, file)
    }
  )
  output$download_link_binary <- downloadHandler(
    filename = function() {
      "bear.png"
    },
    content = function(file) {
      file.copy(bear_img, file)
    }
  )
}

shiny_app <- shinyApp(ui, server)


test_that("download files work from link and button", {
  skip_on_os("windows") # file formats differ on Windows

  app <- AppDriver$new(shiny_app, variant = NULL)

  app$wait_for_js("$('#download_link_csv').attr('href') != ''")
  app$wait_for_js("$('#download_button_csv').attr('href') != ''")
  app$wait_for_js("$('#download_link_txt').attr('href') != ''")
  app$wait_for_js("$('#download_button_txt').attr('href') != ''")
  app$wait_for_js("$('#download_link_binary').attr('href') != ''")
  app$wait_for_js("$('#download_button_binary').attr('href') != ''")

  app$expect_download("download_link_txt")
  app$expect_download("download_button_txt")

  app$expect_download("download_link_csv")
  app$expect_download("download_button_csv")

  app$expect_download("download_link_binary")
  app$expect_download("download_button_binary")

  # Try custom name
  app$expect_download("download_button_txt", name = "my/custom/name.txt")
})


test_that("download files can be retrieved", {
  on.exit(
    {
      if (fs::file_exists("barret.test")) {
        fs::file_delete("barret.test")
      }
    },
    add = TRUE
  )

  app <- AppDriver$new(shiny_app, variant = NULL)

  app$wait_for_js("$('#download_link_csv').attr('href') != ''")
  app$wait_for_js("$('#download_button_csv').attr('href') != ''")

  link_file <- app$get_download("download_link_csv")
  button_file <- app$get_download("download_button_csv", "barret.test")

  expect_equal(fs::path_file(link_file), "download-link.csv")
  expect_equal(fs::path_file(button_file), "barret.test")

  expect_gt(file.info(link_file)$size, 0)
  expect_gt(file.info(button_file)$size, 0)
})


# https://github.com/rstudio/shinytest2/issues/357
test_that("get_download works when AppDriver URL has query parameters", {
  # Reprex from issue #357
  ui <- fluidPage(
    downloadButton("downloadData", "Download")
  )
  server <- function(input, output) {
    data <- mtcars
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data, file)
      }
    )
  }
  my_app <- shinyApp(ui, server)

  # Test with base URL
  test_app1 <- AppDriver$new(my_app)
  current_url <- test_app1$get_url()

  # Test with custom URL containing query parameters
  test_app2 <- AppDriver$new(paste0(current_url, "?foo=bar"))
  csv_file <- test_app2$get_download("downloadData")

  # Should be valid CSV, not an HTML page
  first_line <- readLines(csv_file, n = 1)
  expect_false(
    grepl("<!DOCTYPE", first_line, fixed = TRUE),
    label = "Downloaded file should be CSV, not HTML"
  )
  expect_no_error(read.csv(csv_file))
})
