#' @description
#' It performs one or more update operations via the browser, thens
#' waits for the specified output(s) to update. The test succeeds if
#' all specified output widgets are updated before the `timeout`.
#' For updates that involve a lot of computation, increase the timeout.
#'
#' @param output Name of output control to check.
#' @param ... Name-value pairs used to update inputs.
#' @include shiny-driver.R
ShinyDriver2$set("public", "expectUpdate", function(
  output, ..., timeout = 3000,
  iotype = c("auto", "input", "output")
) {
  # TODO-barret; Should this method be removed? https://github.com/rstudio/shinytest2/issues/18
  stop("not implemented")
  ShinyDriver2$expectUpdate(self, private, output, ..., timeout = timeout,
                    iotype = match.arg(iotype))
})
