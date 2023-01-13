# Saving an app needs all globally defined objects
# See: https://github.com/rstudio/shinytest2/issues/303

# The code below has always worked within `{testthat}` testing
# However, it would break if run in the R console
# https://github.com/rstudio/shinytest2/issues/303#issuecomment-1377950984

# It does not work if run in a `callr::r{_bg}()` process as that is not executed the in their respective global environments
# The only way this error is found is if the code is executed in the global env
# Using `callr::rscript()` requires `{shinytest2}` to be installed
# Therefore, only test on CI as the package is not installed for CRAN testing

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
    ui <- shiny::fluidPage(shiny::textOutput("foo")),
    server = function(input, output, session) {
      foo3
      foo2 <- foo + 1
      output$foo <- shiny::renderText({
        foo2
      })
    }
  )
})

driver <- shinytest2::AppDriver$new(app)

# Keep here to print to `stdout`
print(driver$get_logs())
