

app_get_event_log <- function(self, private) {
  ckm8_assert_app_driver(self, private)
  stop("SHOULD NOT BE USED!")

  log <- private$event_log

  # Log is a row-first list of lists which we need to convert to a data frame.
  # Also, rows don't all have the same column names, so we'll
  all_names <- unique(unlist(lapply(log, names)))
  names(all_names) <- all_names

  vecs <- lapply(all_names, function(nm) {
    col <- lapply(log, `[[`, nm)

    # Replace NULLs with NA so that they don't get lost in conversion from list
    # to vector.
    null_idx <- vapply(col, is.null, logical(1))
    col[null_idx] <- NA
    # Convert to list. Use do.call(c) instead of unlist() because the latter will
    # convert dates and times to numbers.
    do.call(c, col)
  })

  # Add workerId as first column
  vecs <- c(workerid = private$shiny_worker_id, vecs)

  as.data.frame(vecs, stringsAsFactors = FALSE)
}







app_log_message <- function(self, private, messsage) {
  app_add_debug_log_entry(self, private, location = "shinytest2", level = "info", message = messsage)
}



app_debug_log_entry <- function(
  self,
  private,
  location = c("chromote", "shiny", "shinytest2"),
  level,
  message,
  timestamp = Sys.time()
) {
  ckm8_assert_app_driver(self, private)
  checkmate::assert_character(level, len = 1, null.ok = FALSE, any.missing = FALSE)
  checkmate::assert_character(message, len = 1, null.ok = FALSE, any.missing = FALSE)
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
app_add_debug_log_entry <- function(
  self,
  private,
  location,
  level,
  message,
  timestamp = Sys.time()
) {
  ckm8_assert_app_driver(self, private)


  entry <- app_debug_log_entry(
    self, private,
    location = location,
    level = level,
    message = message,
    timestamp = timestamp
  )


  private$debug_log[[length(private$debug_log) + 1]] <- entry

  invisible(entry)
}
