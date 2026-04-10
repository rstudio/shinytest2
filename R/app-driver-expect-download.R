app_download <- function(
  self,
  private,
  output,
  name = NULL,
  save_dir = tempdir(),
  use_counter = FALSE
) {
  ckm8_assert_app_driver(self, private)
  self$log_message(paste0("Downloading file for output id: ", output))

  # Wait for the download element to exist and have a non-empty href.
  # This handles cases where the element is dynamically rendered (e.g., in a
  # modal) or Shiny's output binding hasn't set the href yet. (#445)
  href_js <- paste0(
    "var el = $('#", output, "'); ",
    "el.length > 0 && el.attr('href') !== undefined && el.attr('href') !== ''"
  )
  tryCatch(
    chromote_wait_for_condition(
      self$get_chromote_session(),
      href_js,
      timeout = private$timeout
    ),
    error = function(e) {
      app_abort(
        self, private,
        c(
          paste0("Download from '#", output, "' failed."),
          "i" = "Element not found or its `href` attribute was not set within the timeout.",
          "i" = "If the download button is rendered dynamically (e.g., in a modal), make sure the UI is visible before calling `$get_download()` or `$expect_download()`."
        ),
        parent = e
      )
    }
  )

  # Now fetch the href value
  sub_url <- chromote_eval(
    self$get_chromote_session(),
    paste0("$('#", output, "').attr('href')")
  )$result$value

  # Add the base location to the URL (#357)
  full_url <- private$shiny_url$combine(sub_url)
  req <- app_httr2_get(self, private, full_url)

  # Find suggested name
  content_dispo <- httr2::resp_headers(req)[["content-disposition"]]
  filename_header <- NULL
  if (
    length(content_dispo) == 1 &&
      is.character(content_dispo) &&
      nzchar(content_dispo)
  ) {
    vals <- strsplit(content_dispo, "filename=")[[1]]
    if (length(vals) > 1) {
      filename_header <- gsub("\"", "", vals[2], fixed = TRUE)
      self$log_message(paste0(
        "Content disposition file name: ",
        filename_header
      ))
    }
  }

  include_dash <- TRUE
  filename <-
    if (!is.null(name)) {
      # If a name is provided, use it
      name
    } else if (!is.null(filename_header)) {
      # If a suggested file name is provided, use it
      # Benefit: Snapshot `compare` will be smart if `compare == NULL`
      filename_header
    } else {
      # Use default name
      if (use_counter) {
        # `$expect_download()`
        # (The file counter will be prefixed in the follow chunk of code)
        include_dash <- FALSE
        ".download"
      } else {
        # `$get_download()`
        fs::path_file(tempfile(fileext = ".download"))
      }
    }
  # Prefix the downloaded file to make it uniquely named
  # Must be implemented until https://github.com/r-lib/testthat/pull/1592 is addressed
  if (use_counter) {
    dash <- if (include_dash) "-" else ""
    filename <- sprintf("%03d%s%s", private$counter$increment(), dash, filename)
  }

  download_path <- fs::path(
    # Save file to temp app dir
    save_dir,
    # Don't allow weird file paths from the application
    fs::path_sanitize(filename, "_")
  )
  # Save contents
  writeBin(httr2::resp_body_raw(req), download_path)

  list(
    download_path = download_path,
    filename_header = filename_header
  )
}


app_expect_download <- function(
  self,
  private,
  output,
  ...,
  compare = NULL,
  name = NULL,
  transform = NULL
) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  ## Announce snapshot file
  # Cannot announce before download because we need the filename
  # from the Content-Disposition header... which is after we download the file
  # Therefore, we cannot announce the testthat file

  snapshot_info <- app_download(
    self,
    private,
    output = output,
    name = name,
    # Save within temp app dir
    save_dir = private$save_dir,
    # Ex: 003.download
    use_counter = TRUE
  )

  # Compare download_file content
  app__expect_snapshot_file(
    self,
    private,
    snapshot_info$download_path,
    transform = transform,
    compare = compare
  )

  # Compare requested filename
  requested_download_name <- snapshot_info$filename_header
  if (!is.null(requested_download_name)) {
    app__expect_snapshot_value(
      self,
      private,
      requested_download_name
    )
  }

  invisible(self)
}


app_get_download <- function(
  self,
  private,
  output,
  filename = NULL
) {
  ckm8_assert_app_driver(self, private)
  # rlang::check_dots_empty()

  if (is.null(filename)) {
    # Save to a temporary file with possibly suggested name
    save_dir <- tempdir()
    name <- NULL
  } else {
    # Use provided filename info
    save_dir <- fs::path_dir(filename)
    name <- fs::path_file(filename)
  }

  snapshot_info <- app_download(
    self,
    private,
    output = output,
    name = name,
    save_dir = save_dir,
    use_counter = FALSE
  )

  snapshot_info$download_path
}
