app_httr_get <- function(self, private, url, fn_404 = NULL) {
  ckm8_assert_app_driver(self, private)

  pieces <- httr::parse_url(url)
  # Add in port information if it's missing
  # https://github.com/rstudio/shinytest2/issues/158
  if (is.null(pieces$port)) {
    pieces$port <- switch(pieces$scheme, "http" = 80, "https" = 443)
  }

  if (!pingr::is_up(pieces$hostname, pieces$port)) {
    app_abort(self, private, "Could not find Shiny server. Shiny app is no longer running")
  }

  withCallingHandlers( # abort() on error
    {
      req <- httr::GET(url)
    },
    # Attempt to capture empty reply error and provide better message
    error = function(e) {
      if (grepl("Empty reply from server", as.character(e), fixed = TRUE)) {
        app_abort(self, private, "Empty reply received from Shiny server. Shiny app is no longer running", parent = e)
      }
      # Unknown error, rethrow
      app_abort(self, private, e)
    }
  )

  status <- httr::status_code(req)
  if (status == 200) {
    return(req)
  }
  if (status == 404 && is.function(fn_404)) {
    return(fn_404(req))
  }

  cat("{shinytest2} query failed (", status, ")----------------------\n", sep = "")
  cat("URL: ", url, "\n", sep = "")
  cat(httr::content(req, "text"), "\n")
  cat("----------------------------------------\n")
  app_abort(self, private, "Unable request data from server")
}
