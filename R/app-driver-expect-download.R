
app_download <- function(
  self, private,
  id, name = NULL
) {
  ckm8_assert_app_driver(self, private)

  temp_save_dir <- private$appshot_dir
  snapshot_count <- private$appshot_count$increment()

  if (is.null(name)) {
    name <- sprintf("%03d.download", snapshot_count)
  }

  self$log_message("Downloading file: ", name)

  # Find the URL to download from (the href of the <a> tag)
  sub_url <- chromote_eval(self$get_chromote_session(), paste0("$('#", id, "').attr('href')"))$result$value
  if (identical(sub_url, "")) {
    abort(paste0("Download from '#", id, "' failed"), app = self)
  }
  # Add the base location to the URL
  full_url <- paste0(private$shiny_url$get(), sub_url)
  req <- httr_get(full_url)

  download_path <- fs::path(temp_save_dir, name)
  create_snapshot_dir(temp_save_dir, snapshot_count)
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
  testthat_expect_snapshot_file(
    private,
    snapshot_info$download_path,
    cran = cran,
    compare = testthat::compare_file_text
  )

  invisible(self)
}
