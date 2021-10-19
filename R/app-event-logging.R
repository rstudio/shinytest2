#' @include shiny-driver.R
ShinyDriver2$set("private", "eventLog", list())

#' @description Add event to log.
#' @param event Event name
#' @param ... Addition data to store for event
#' @include shiny-driver.R
ShinyDriver2$set("public", "logEvent", function(event, ...) {

  content <- list(time = Sys.time(), event = event, ...)
  stopifnot(rlang::is_named(content))

  private$eventLog[[length(private$eventLog) + 1]] <- content
  invisible(self)
})

#' @description Retrieve event log.
#' @include shiny-driver.R
ShinyDriver2$set("public", "getEventLog", function() {
  log <- private$eventLog

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
  vecs <- c(workerid = private$shinyWorkerId, vecs)

  as.data.frame(vecs, stringsAsFactors = FALSE)
})
