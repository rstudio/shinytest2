

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
compare_file_screenshot <- function(old, new, tolerance = 1) {
  rlang::check_installed("png")

  tolerance <- as.integer(tolerance)
  checkmate::assert_integer(tolerance, lower = 0, upper = 255, len = 1, any.missing = FALSE)

  # Read in pixel data to a matrix
  old_png <- suppressWarnings(png::readPNG(old))
  new_png <- suppressWarnings(png::readPNG(new))

  # Quit early if images are different size
  if (
    nrow(old_png) != nrow(new_png) ||
    ncol(old_png) != ncol(new_png)
  ) {
    return(FALSE)
  }


  # Convert from decimal rgb hex with as.raster
  # Convert from hex to rgb matrix with col2rgb
  old_rgb <- grDevices::col2rgb(grDevices::as.raster(old_png), alpha = TRUE)
  new_rgb <- grDevices::col2rgb(grDevices::as.raster(new_png), alpha = TRUE)

  # If only certain channels are returned, make sure they are the same
  if (all(rownames(old_rgb) != rownames(new_rgb))) {
    return(FALSE)
  }

  # For each color channel...
  for (i in seq_along(nrow(old_rgb))) {
    abs_diff <- abs(old_rgb[i, ] - new_rgb[i, ])

    # Allow a tolerance of _1_ unit in either direction (default)
    if (any(abs_diff > tolerance)) {
      return(FALSE)
    }
  }

  # No bad diffs found
  return(TRUE)
}
