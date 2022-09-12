
app_stop <- function(self, private) {
  "!DEBUG AppDriver$stop"
  ckm8_assert_app_driver(self, private)

  if (private$state == "stopped") {
    return(invisible(private$shiny_proc_value))
  }

  self$log_message("Closing chromote session")
  tryCatch(
    self$get_chromote_session()$close(),
    error = function(e) {
      self$log_message(paste0("Error closing chromote session!\n", conditionMessage(e)))
    }
  )

  # If the app is being hosted locally, kill the process.
  if (!is.null(private$shiny_process)) {
    if (private$shiny_process$is_alive()) {
      self$log_message("Ending Shiny process. Ignoring Shiny process result")

      tryCatch(
        {
          # Attempt soft-kill before hard-kill. This is a workaround for
          # https://github.com/r-lib/processx/issues/95
          # SIGINT quits the Shiny application, SIGTERM tells R to quit.
          # Unfortunately, SIGTERM isn't quite the same as `q()`, because
          # finalizers with onexit=TRUE don't seem to run.

          # Using private$shiny_process$interrupt() to send SIGNAL 2 (SIGINT) to the process
          # https://github.com/r-lib/processx/blob/84301784382296217e7f5d11e1116dc4e24da809/R/process.R#L276-L283
          # https://github.com/r-lib/covr/issues/277#issuecomment-555502769
          # https://github.com/rstudio/shinytest2/issues/250

          # Increased the timeout for packages like covr to upload their results:
          #   https://github.com/rstudio/shinytest2/issues/250
          private$shiny_process$interrupt()
          private$shiny_process$wait(10 * 1000)

          private$shiny_process$signal(tools::SIGTERM)
          private$shiny_process$wait(10 * 1000)

          private$shiny_process$kill()
        },
        error = function(e) {
          self$log_message(paste0("Error closing Shiny process!\n", conditionMessage(e)))
        }
      )
    } else {
      # Store the value to return later
      self$log_message("Getting Shiny process result")
      tryCatch(
        private$shiny_proc_value <- private$shiny_process$get_result(),
        error = function(e) {
          self$log_message(paste0("Error getting Shiny process result!\n", conditionMessage(e)))
        }
      )
    }
  }

  private$state <- "stopped"
  invisible(private$shiny_proc_value)
}



# Function run on garbage collection
app_finalize <- function(
  self, private
) {
  ckm8_assert_app_driver(self, private)

  # Stop the app
  self$stop()

  # Chromote has its own cleanup process on finalize

  if (isTRUE(private$clean_logs) && !is.null(private$shiny_process)) {
    unlink(private$shiny_process$get_output_file())
    unlink(private$shiny_process$get_error_file())
  }

  # Can not remove snapshot files in the same function that they are created,
  #   so it is safer to clean up the files when the app is not needed
  #   (Not that big of a memory leak for a single app)
  unlink(private$save_dir, recursive = TRUE)
}
