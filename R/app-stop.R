#' @include shiny-driver.R


#' @description
#' Stop the app, the terminate external R process that runs the app and
#' the phantomjs instance.
#' @include shiny-driver.R
ShinyDriver2$set("public", "stop", function() {
  "!DEBUG ShinyDriver2$stop"

  if (private$state == "stopped")
    return(invisible(self))

  self$log_event("Closing chromote session")
  # private$web$delete()
  self$get_chromote_session()$close()

  # If the app is being hosted locally, kill the process.
  if (!is.null(private$shiny_process)) {
    self$log_event("Ending Shiny process")

    # Attempt soft-kill before hard-kill. This is a workaround for
    # https://github.com/r-lib/processx/issues/95
    # SIGINT quits the Shiny application, SIGTERM tells R to quit.
    # Unfortunately, SIGTERM isn't quite the same as `q()`, because
    # finalizers with onexit=TRUE don't seem to run.
    private$shiny_process$signal(tools::SIGINT)
    private$shiny_process$wait(500)
    private$shiny_process$signal(tools::SIGTERM)
    private$shiny_process$wait(250)
    private$shiny_process$kill()
  }

  private$state <- "stopped"
  invisible(self)
})


#' @include shiny-driver.R
ShinyDriver2$set("private", "should_clean_logs", TRUE) # Whether to clean logs when GC'd

# Function run on garbage collection
#' @include shiny-driver.R
ShinyDriver2$set("private", "finalize", function() {

  # Stop the app
  self$stop()

  # Chromote has its own cleanup process on finalize

  if (isTRUE(private$should_clean_logs) && !is.null(private$shiny_process)) {
    unlink(private$shiny_process$get_output_file())
    unlink(private$shiny_process$get_error_file())
  }

  # Can not remove snapshot files in the same function that they are created,
  #   so it is safer to clean up the files when the app is not needed
  #   (Not that big of a memory leak for a single app)
  unlink(private$appshot_dir, recursive = TRUE)
})
