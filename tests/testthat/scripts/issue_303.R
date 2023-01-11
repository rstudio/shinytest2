# Saving an app needs all globally defined objects
# See: https://github.com/rstudio/shinytest2/issues/303

# The code below has always worked within `{testthat}` testing
# However, it would break if run in the R console
# https://github.com/rstudio/shinytest2/issues/303#issuecomment-1377950984

# driver$get_logs()
#> {shiny}      R  stderr ----------- Warning: Error in server: object 'foo' not found
#> {shiny}      R  stderr -----------   60: server [dean.R#11]
#> {shiny}      R  stderr -----------   14: <Anonymous>
#> {shiny}      R  stderr -----------   12: <Anonymous>
#> {shiny}      R  stderr -----------   11: <Anonymous>
#> {shiny}      R  stderr -----------   10: do.call
#> {shiny}      R  stderr -----------    9: saveRDS
#> {shiny}      R  stderr -----------    8: withCallingHandlers
#> {shiny}      R  stderr -----------    7: doTryCatch
#> {shiny}      R  stderr -----------    6: tryCatchOne
#> {shiny}      R  stderr -----------    5: tryCatchList
#> {shiny}      R  stderr -----------    2: tryCatchList
#> {shiny}      R  stderr -----------    1: tryCatch
#> {shiny}      R  stderr ----------- Error in server(...) : object 'foo' not found

foo <- 5
app <- local({

  foo3 <- 19

  shiny::shinyApp(
    ui <- shiny::fluidPage(textOutput("foo")),
    server = function(input, output, session) {
      foo3
      foo2 <- foo + 1
      output$foo <- renderText({
        foo2
      })
    }
  )
})

driver <- AppDriver$new(app)
driver$get_logs()
