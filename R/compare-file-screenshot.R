

#' Compare screenshots given tolerance
#'
#' \pkg{chromote} produces screenshot images with inconsistent rounding for areas like
#' the corner of a text area. This function compares two images and allows for a `tolerance`
#' of so many units in each RGBA color channel.
#'
#' It is suggested to use this method with [`AppDriver`]`$expect_screenshot()` to compare
#' screenshots given a particular tolerance.
#'
#' @param old Current screenshot file
#' @param new New screenshot file
#' @param threshold How many units of difference to allow
#' # TODO-barret; maybe add flag for fast/slow? Default to fast=TRUE? Don't even give option and always do slow?
#' @export
compare_screenshot_threshold <- function(threshold = NULL, kernal_size = 5) {
  function(old, new, ) {

    is_same_file <- testthat::compare_file_binary(old, new)
    # Quit early if they are the same file
    if (is_same_file) return(TRUE)

    # If no tolerance value is provided, return the previous answer of `is_same_file`
    if (is.null(tolerance)) {
      return(is_same_file)
    }

    # A tolerance value is provided, convolve the images

    rlang::check_installed("png")

    tolerance <- as.integer(tolerance)
    checkmate::assert_integer(tolerance, lower = 0, upper = 255, len = 1, any.missing = FALSE)

    # Read in pixel data to a matrix
    old_png <- suppressWarnings(png::readPNG(old))
    new_png <- suppressWarnings(png::readPNG(new))

    # Quit early if images are different size
    if (!identical(dim(old_png), dim(new_png))) {
      return(FALSE)
    }

    diff_matrix <- rowSums(abs(old_png - new_png), dims = 2)

    conv_max_value <- screenshot_difference_internal(diff_matrix, kernel_size = kernal_size, threshold = tolerance)

    expect_lt(conv_max_value, tolerance)


    # TOOD-barret; remove
    str(conv_max_value)

    conv_max_value < tolerance
  }
}


# TODO-barret; export method to help users find the currrent break point
max_screenshot_difference <- function(old, new, kernal_size = 5) {
  screenshot_difference_internal(
    old, new,
    # Tell cpp11 to provide the max value found
    threshold = -1
  )
}

screenshot_difference_internal <- function(old, new, kernal_size = 5, threshold = -1) {
  if (threshold != -1) {
    threshold <- as.double(threshold)
    checkmate::assert_double(threshold, lower = 0, finite = TRUE, any.missing = FALSE, len = 1)
  } else {
    threshold <- as.double(threshold)
  }
  kernal_size <- as.integer(kernal_size)
  checkmate::assert_integer(kernal_size, lower = 1, finite = TRUE, any.missing = FALSE, len = 1)

  image_diff_convolution_max_value(diff_matrix, kernel_size = kernal_size, threshold = threshold)
}
