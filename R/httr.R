httr_get <- function(url) {
  pieces <- httr::parse_url(url)
  # Only check if no basic auth is being used
  if (is.null(pieces$username) && is.null(pieces$password)) {
    if (!pingr::is_up(pieces$hostname, pieces$port)) {
      abort("Shiny app is no longer running")
    }
  }

  withCallingHandlers(
    {
      req <- httr::GET(url)
    },
    # Attempt to capture empty reply error and provide better message
    error = function(e) {
      if (grepl("Empty reply from server", as.character(e), fixed = TRUE)) {
        abort("Shiny app is no longer running", parent = e)
      }
      # Unknown error, rethrow
      abort(e)
    }
  )

  status <- httr::status_code(req)
  if (status == 200) {
    return(req)
  }

  cat("Query failed (", status, ")----------------------\n", sep = "")
  cat(httr::content(req, "text"), "\n")
  cat("----------------------------------------\n")
  abort("Unable request data from server")
}
