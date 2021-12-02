testthat_expect_snapshot_value <- function( # nolint
  private,
  x,
  cran = FALSE
) {
  testthat::expect_snapshot_value(
    x,
    cran = cran,
    variant = private$variant
  )
}
testthat_expect_snapshot_file <- function(
  private,
  file,
  name = fs::path_file(file),
  cran = FALSE,
  compare = testthat::compare_file_binary
) {
  # Add name prefix to saved snapshot file
  name <-
    if (is.null(private$name)) {
      name
    } else {
      paste0(private$name, "-", name)
    }

  testthat::expect_snapshot_file(
    file,
    name = name,
    cran = cran,
    compare = compare,
    variant = private$variant
  )
}



app_expect_appshot <- function(
  self, private,
  ...,
  name = NULL,
  items = NULL,
  screenshot = NULL,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  appshot_info <- app_appshot(self, private, items = items, name = name, screenshot = screenshot)

  # compare json
  testthat_expect_snapshot_file(
    private,
    appshot_info$json_path,
    cran = cran,
    compare = testthat::compare_file_text
  )

  # compare screenshot
  if (!is.null(appshot_info$screenshot_path)) {
    testthat_expect_snapshot_file(
      private,
      appshot_info$screenshot_path,
      cran = cran,
      compare = testthat::compare_file_binary
    )
  }

  invisible(self)
}
