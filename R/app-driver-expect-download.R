
app_download <- function(
  self, private,
  id,
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
  sub_url <- chromote_eval(self$get_chromote_session(), paste0("$('#", id, "').attr('href')"))$result$value
  if (identical(sub_url, "")) {
    app_abort(self, private, paste0("Download from '#", id, "' failed"))
  }
  # Add the base location to the URL
  full_url <- paste0(private$shiny_url$get(), sub_url)
  req <- app_httr_get(self, private, full_url)

  download_path <- fs::path(private$save_dir, name)
  writeBin(req$content, download_path)

  list(
    download_path = download_path
  )
}


app_expect_download <- function(
  self,
  private,
  id,
  ...,
  name = NULL,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  snapshot_info <- app_download(self, private, id = id, name = name)

  # compare download_file
  app__expect_snapshot_file(
    self, private,
    snapshot_info$download_path,
    cran = cran,
    compare = testthat::compare_file_text
  )

  invisible(self)
}


app_get_download <- function(
  self, private,
  id,
  filename = NULL
) {
  ckm8_assert_app_driver(self, private)
  # ellipsis::check_dots_empty()

  filename <- filename %||% tempfile(fileext = ".download")
  ckm8_assert_single_string(filename)

  snapshot_info <- app_download(self, private, id = id, name = fs::path_file(tempfile()))
  fs::file_copy(snapshot_info$download_path, filename, overwrite = TRUE)

  filename
}
