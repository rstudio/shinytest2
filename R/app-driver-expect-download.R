
app_download <- function(
  self, private,
  output,
  name = NULL
) {
  ckm8_assert_app_driver(self, private)

  if (is.null(name)) {
    name <- sprintf("%03d.download", private$counter$increment())
  }
  if (fs::path_file(name) != name) {
    app_abort(self, private,
      paste0("Download file name must be a single name location, not a full path. Received: ", name)
    )
  }

  self$log_message(paste0("Downloading file: ", name))

  # Find the URL to download from (the href of the <a> tag)
  sub_url <- chromote_eval(self$get_chromote_session(), paste0("$('#", output, "').attr('href')"))$result$value
  if (identical(sub_url, "")) {
    app_abort(self, private, paste0("Download from '#", output, "' failed"))
  }
  # Add the base location to the URL
  full_url <- paste0(private$shiny_url$get(), sub_url)
  req <- app_httr_get(self, private, full_url)

  download_path <- fs::path(private$save_dir, name)
  writeBin(req$content, download_path)

  content_dispo <- httr::headers(req)[["content-disposition"]]
  filename_header <- NULL
  if (length(content_dispo) == 1 && is.character(content_dispo) && nzchar(content_dispo)) {
    vals <- strsplit(content_dispo, "filename=")[[1]]
    if (length(vals) > 1) {
      filename_header <- gsub("\"", "", vals[2], fixed = TRUE)
      self$log_message(paste0("Content disposition file name: ", filename_header))
    }
  }

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
  name = NULL,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  snapshot_info <- app_download(self, private, output = output, name = name)

  # Compare download_file content
  app__expect_snapshot_file(
    self, private,
    snapshot_info$download_path,
    cran = cran,
    compare = testthat::compare_file_text
  )

  # Compare requested filename
  requested_download_name <- snapshot_info$filename_header
  if (!is.null(requested_download_name)) {
    app__expect_snapshot_value(
      self, private,
      requested_download_name,
      cran = cran
    )
  }

  invisible(self)
}


app_get_download <- function(
  self, private,
  output,
  filename = NULL
) {
  ckm8_assert_app_driver(self, private)
  # ellipsis::check_dots_empty()

  snapshot_info <- app_download(self, private, output = output, name = fs::path_file(tempfile()))

  filename <-
    if (!is.null(filename))
      filename
    else if (!is.null(snapshot_info$filename_header))
      # Don't allow weird file paths from the application
      fs::path(tempfile(), fs::path_sanitize(snapshot_info$filename_header, "_"))
    else
      tempfile(fileext = ".download")
  ckm8_assert_single_string(filename)

  fs::dir_create(fs::path_dir(filename), recurse = TRUE)
  fs::file_copy(snapshot_info$download_path, filename, overwrite = TRUE)
  fs::file_delete(snapshot_info$download_path)

  filename
}
