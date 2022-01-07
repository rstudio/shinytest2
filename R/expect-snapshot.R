app__expect_snapshot_value <- function( # nolint
  self, private,
  x,
  cran = FALSE,
  ...
) {
  ckm8_assert_app_driver(self, private)
  testthat::expect_snapshot_value(
    x,
    cran = cran,
    variant = private$variant,
    ...
  )
}
app__expect_snapshot_file <- function(
  self, private,
  file,
  variant,
  name = fs::path_file(file),
  cran = FALSE,
  compare = testthat::compare_file_binary
) {
  ckm8_assert_app_driver(self, private)

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
