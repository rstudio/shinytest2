
sd2_expectSnapshot <- function(
  self,
  private,
  ...,
  name = NULL,
  items = NULL,
  screenshot = NULL,
  cran = FALSE,
  error = FALSE,
  variant = NULL
) {
  testthat::expect_s3_class(self, "ShinyDriver2")
  ellipsis::check_dots_empty()

  variant <- variant %||% private$variant

  snapshot_info <- sd2_snapshot(self, private, items = items, name = name, screenshot = screenshot)
  # utils::str(snapshot_info)

  add_self_name_prefix <- function(x) {
    x <- fs::path_file(x)
    if (is.null(private$name)) {
      x
    } else {
      paste0(private$name, "-", x)
    }
  }

  # compare json
  testthat::expect_snapshot_file(
    snapshot_info$json_path,
    name = add_self_name_prefix(snapshot_info$json_path),
    cran = cran,
    compare = testthat::compare_file_text,
    variant = variant
  )

  # compare screenshot
  if (!is.null(snapshot_info$screenshot_path)) {
    testthat::expect_snapshot_file(
      snapshot_info$screenshot_path,
      name = add_self_name_prefix(snapshot_info$screenshot_path),
      cran = cran,
      compare = testthat::compare_file_binary,
      variant = variant
    )
  }

}


#' Expect a shinytest2 snapshot
#'
#'
#' @param app A [ShinyDriver2] object.
#' @param ... Must be empty. Allows for parameter expansion.
#' @param name The prefix name to be used for the snapshot. By default, this uses the name supplied to `app` on initialization.
#' @param items Elements to only be included in the snapshot. If supplied, can contain `inputs`, `output`, and `export`. Each value of `items` can either be `TRUE` (for all values) or a character list of names to use.
#' @param screenshot A boolean indicating whether to take a screenshot.
#' @param variant A character vector of the form `[os_name]-[r_version]` indicating the operating system and R version (`major.minor`) being tested.
#' @inheritParams testthat::expect_snapshot_file
#' @export
expect_st2_snapshot <- function(
  app,
  ...,
  name = NULL,
  items = NULL,
  screenshot = NULL,
  cran = FALSE,
  variant = NULL
) {
  app$expectSnapshot(
    ...,
    name = name,
    items = items,
    screenshot = screenshot,
    cran = cran,
    variant = variant
  )

}
