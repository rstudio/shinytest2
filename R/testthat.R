
sd2_expectSnapshot <- function(
  self,
  private,
  ...,
  name = NULL,
  items = NULL,
  screenshot = NULL,
  cran = FALSE,
  error = FALSE,
  transform = NULL,
  variant = getOption("shinytest2.variant", os_name_and_r_version())
) {
  testthat::expect_s3_class(self, "ShinyDriver2")
  ellipsis::check_dots_empty()

  snapshot_info <- sd2_snapshot(self, private, items = items, name = name, screenshot = screenshot)
  # utils::str(snapshot_info)

  add_self_name_prefix <- function(x) {
    x <- fs::path_file(x)
    if (is.null(self$name)) {
      x
    } else {
      paste0(self$name, "-", x)
    }
  }

  name_prefix <-

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


#' Test Shinytest2 code chunk
#'
#' Used to shim in extra information (platform and R version) into the `desc`ription of the test.
#'
#' Since `shinytest::test_that` will know which platform is being run, other platform's test snaps will not be auto-deleted due to not being used.
#'
#' @inheritParams testthat::test_that
#' @export
expect_st2_snapshot <- function(
  app,
  ...,
  name = NULL,
  items = NULL,
  screenshot = NULL,
  cran = FALSE,
  # error = FALSE,
  transform = NULL,
  variant = os_name_and_r_version()
) {
  app$expectSnapshot(
    ...,
    name = name,
    items = items,
    screenshot = screenshot,
    cran = cran,
    transform = transform,
    variant = variant
  )

}
