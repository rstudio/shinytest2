#' @include shiny-driver.R


#' @description
#' Stop the app, the terminate external R process that runs the app and
#' the phantomjs instance.
#' @include shiny-driver.R
ShinyDriver2$set("public", "stop", function() {
  "!DEBUG ShinyDriver2$stop"

  if (private$state == "stopped")
    return(invisible(self))

  self$logEvent("Closing Chrome session")
  # private$web$delete()
  self$chromote_session$close()

  # If the app is being hosted locally, kill the process.
  if (!is.null(private$shinyProcess)) {
    self$logEvent("Ending Shiny process")

    # Attempt soft-kill before hard-kill. This is a workaround for
    # https://github.com/r-lib/processx/issues/95
    # SIGINT quits the Shiny application, SIGTERM tells R to quit.
    # Unfortunately, SIGTERM isn't quite the same as `q()`, because
    # finalizers with onexit=TRUE don't seem to run.
    private$shinyProcess$signal(tools::SIGINT)
    private$shinyProcess$wait(500)
    private$shinyProcess$signal(tools::SIGTERM)
    private$shinyProcess$wait(250)
    private$shinyProcess$kill()
  }

  private$state <- "stopped"
  invisible(self)
})


#' @include shiny-driver.R
ShinyDriver2$set("private", "cleanLogs", TRUE) # Whether to clean logs when GC'd

# Function run on garbage collection
#' @include shiny-driver.R
ShinyDriver2$set("private", "finalize", function() {

  # Stop the app
  self$stop()

  # Chromote has its own cleanup process on finalize

  if (isTRUE(private$cleanLogs)) {
    unlink(private$shinyProcess$get_output_file())
    unlink(private$shinyProcess$get_error_file())
  }

  # TODO-barret-question; Remove temp dir when garbage collected?
  # TODO-barret-ansewr; REmove the files once they have been compared. No need for a temp directory in the first place
})
