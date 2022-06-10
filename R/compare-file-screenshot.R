

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
#' @param tolerance How many units of difference to allow
#' @export
compare_file_screenshot <- function(old, new, tolerance = 10) {
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

  breaks_threshold <- image_diff_breaks_threshold(diff_matrix, kernel_size = 5, threshold = tolerance)
  return(!breaks_threshold)
}
