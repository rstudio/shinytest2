

#' Compare screenshots given threshold value
#'
#' \pkg{chromote} produces screenshot images with inconsistent rounding for areas like
#' the corner of a text area. This function compares two images and allows for a `threshold`
#' of so many units in each RGBA color channel.
#'
#' It is suggested to use this method with [`AppDriver`]`$expect_screenshot()` to compare
#' screenshots given a particular threshold value.
#'
#' @param old Current screenshot file
#' @param new New screenshot file
#' @param ... Must be empty. Allows for parameter expansion.
#' @param threshold How many units of difference to allow
#' @param kernel_size How many pixels tall and wide should the convolution be applied to
#' @export
compare_screenshot_threshold <- function(old, new, ..., threshold = NULL, kernel_size = 5) {
  ellipsis::check_dots_empty()

  is_same_file <- testthat::compare_file_binary(old, new)
  # Quit early if they are the same file
  if (is_same_file) return(TRUE)

  # If no threshold value is provided, return the previous answer of `is_same_file`
  if (is.null(threshold)) {
    return(is_same_file)
  }

  # A threshold value is provided, convolve the images

  threshold <- as.double(threshold)
  checkmate::assert_double(threshold, lower = 0, finite = TRUE, any.missing = FALSE, len = 1)

  conv_max_value <- screenshot_max_difference(
    old = old,
    new = new,
    kernel_size = kernel_size
  )

  conv_max_value < threshold
}


#' @export
screenshot_max_difference <- function(
  old,
  new,
  ...,
  kernel_size = 5
) {
  checkmate::assert_file_exists(old)
  checkmate::assert_file_exists(new)

  kernel_size <- as.integer(kernel_size)
  checkmate::assert_integer(
    kernel_size,
    lower = 1,
    any.missing = FALSE,
    len = 1
  )

  # A threshold value is provided, convolve the images
  rlang::check_installed("png")
  # Read in pixel data to a matrix
  old_png <- suppressWarnings(png::readPNG(old))
  new_png <- suppressWarnings(png::readPNG(new))

  # Quit early if images are different size
  if (!identical(dim(old_png), dim(new_png))) {
    rlang::abort("Images are different dimensions")
  }

  # diff_png <- abs(old_png - new_png)
  # bench::mark(
  #   rowsums = { rowSums(diff_png, dims = 2)},
  #   manual = {
  #     diff_matrix <- matrix(0, nrow = nrow(diff_png), ncol = ncol(diff_png))
  #     for (i in seq_len(dim(diff_png)[3])) {
  #       diff_matrix <- diff_matrix + diff_png[,,i]
  #     }
  #     diff_matrix
  #   }
  # )

  # Per pixel location, sum up each channel diff
  diff_matrix <- rowSums(abs(old_png - new_png), dims = 2)

  # Use cpp11!
  conv_max_value <- image_diff_convolution_max_value(
    diff_matrix,
    kernel_size = kernel_size
  )

  conv_max_value
}
