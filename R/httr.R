httr_get <- function(url) {
  pieces <- httr::parse_url(url)

  cat("Url parts: \n")
  utils::str(list(
    url = url,
    pieces = pieces
  ))

  if (!pingr::is_up(pieces$hostname, pieces$port)) {
    stop("Shiny app is no longer running")
  }

  tryCatch(
    {
      req <- httr::GET(url)
    },
    # Attempt to capture empty reply error and provide better message
    error = function(e) {
      error_text <- as.character(e)
      if (grepl("Empty reply from server", as.character(e), fixed = TRUE)) {
        stop("Shiny app is no longer running")
      }
      # Unknown error, rethrow
      stop(e)
    }
  )

  status <- httr::status_code(req)
  if (status == 200) {
    return(req)
  }

  cat("Query failed (", status, ")----------------------\n", sep = "")
  cat(httr::content(req, "text"), "\n")
  cat("----------------------------------------\n")
  stop("Unable request data from server")
}
