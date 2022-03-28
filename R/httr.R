app_httr_get <- function(self, private, url) {
  ckm8_assert_app_driver(self, private)

  pieces <- httr::parse_url(url)
  if (!pingr::is_up(pieces$hostname, pieces$port)) {
    app_abort(self, private, "Shiny app is no longer running")
  }

  withCallingHandlers(
    {
      req <- httr::GET(url)
    },
    # Attempt to capture empty reply error and provide better message
    error = function(e) {
      if (grepl("Empty reply from server", as.character(e), fixed = TRUE)) {
        app_abort(self, private, "Shiny app is no longer running", parent = e)
      }
      # Unknown error, rethrow
      app_abort(self, private, e)
    }
  )

  status <- httr::status_code(req)
  if (status == 200) {
    return(req)
  }

  cat("{shinytest2} query failed (", status, ")----------------------\n", sep = "")
  cat("URL: ", url, "\n", sep = "")
  cat(httr::content(req, "text"), "\n")
  cat("----------------------------------------\n")
  app_abort(self, private, "Unable request data from server")
}
