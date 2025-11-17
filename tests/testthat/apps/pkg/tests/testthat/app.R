message("app-loaded pkgs")
message(paste0(collapse = "\n", capture.output(pkgload:::dev_packages())))

message(paste0(collapse = "\n", capture.output(searchpaths())))

testthat::expect_error(expkg_exported_value, "not found")

# str(pkgload:::dev_packages())
# str(rlang::pkg_env("expkg"))

print(library)

expect_true(require(expkg)) # This also attaches the package
# library(expkg) # This would work too!

message("app-post-library pkgs")
message(paste0(collapse = "\n", capture.output(pkgload:::dev_packages())))

message(paste0(collapse = "\n", capture.output(searchpaths())))

testthat::expect_equal(expkg_exported_value, "expkg exported value")

testthat::expect_error(expkg_internal_value, "not found")
testthat::expect_equal(expkg:::expkg_internal_value, "expkg internal value")

# testthat::expect_equal(expkg_internal_value, "expkg internal value")
# testthat::expect_equal(expkg_exported_value, "expkg exported value")

app <- shinyApp(
  ui = fluidPage(
    tags$h1("Local Package Example"),
    tags$p("If this app loads, it is a success!"),
    actionButton("btn", "Click Me"),
    verbatimTextOutput("btn_click_count")
  ),
  server = function(input, output) {
    output$btn_click_count <- renderText({
      input$btn
    })
  }
)
