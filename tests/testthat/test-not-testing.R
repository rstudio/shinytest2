skip_on_cran()
require("shiny", quietly = TRUE, character.only = TRUE)

test_that("Running an app not in testing mode has 404 handled when getting values", {

  app_bg <- callr::r_bg(
    function() {
      shiny::runApp(
        shinyApp("", function(input, output) {}),
        test.mode = FALSE
      )
    },
    stderr = "|"
  )
  withr::defer({ app_bg$kill() })

  # Wait until Shiny server is running
  app_url <- NULL
  for (i in 1:100) {
    lines <- paste0(app_bg$read_error_lines(), collapse = "\n")
    if (length(lines) > 0) {
      if (grepl("http", lines, fixed = TRUE)) {
        lines <- strsplit(lines, "\n")[[1]]
        line <- lines[grepl("http", lines, fixed = TRUE)]
        app_url <- tail(strsplit(line, " ")[[1]], 1)
        break
      }
    }
    Sys.sleep(0.1)
  }

  expect_true(!is.null(app_url))

  app <- AppDriver$new(app_url)
  withr::defer({ app$stop() })

  expect_error(
    app$get_values(),
    "Shiny server returned 404"
  )

  expect_error(
    app$expect_values(),
    "Shiny server returned 404"
  )
})
