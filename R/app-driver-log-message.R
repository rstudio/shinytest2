
app_log_message <- function(self, private, message) {
  app_add_log_entry(self, private, location = "shinytest2", level = "info", message = message)
}



app_log_entry <- function(
  self,
  private,
  location = c("chromote", "shiny", "shinytest2"),
  level,
  message,
  timestamp = Sys.time()
) {
  ckm8_assert_app_driver(self, private)
  ckm8_assert_single_string(level)
  ckm8_assert_single_string(message)
  checkmate::assert_posixct(timestamp)

  entry <- data.frame(
    workerid = private$shiny_worker_id,
    timestamp = timestamp,
    location = match.arg(location),
    # do not match on a fixed set to allow console.trace() methods to work in browser.
    level,
    message = message
  )
  # message("Making entry: ")
  # str(entry)
  entry
}
app_add_log_entry <- function(
  self,
  private,
  location,
  level,
  message,
  timestamp = Sys.time()
) {
  ckm8_assert_app_driver(self, private)


  entry <- app_log_entry(
    self, private,
    location = location,
    level = level,
    message = message,
    timestamp = timestamp
  )


  private$log[[length(private$log) + 1]] <- entry

  invisible(entry)
}
