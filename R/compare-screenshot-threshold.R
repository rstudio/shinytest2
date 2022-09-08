

#' Compare screenshots given threshold value
#'
#' \pkg{chromote} can sometimes produce screenshot images with non-deterministic
#' (yet close) color values. This can happen in locations such as rounded
#' corners of `div`s or `textarea`s.
#'
#' These differences make comparing screenshots impractical using traditional
#' expectation methods as false-positives are produced often over time. To
#' mitigate this, we can use a _fuzzy matching_ algorithm that can tolerate
#' small regional differences throughout the image. If the local changes found
#' are larger than the `threshold`, then the images are determined to be
#' different. Both the screenshot difference `threshold` and the size of the
#' kernel (`kernel_size`) can be set to tune the false positive rate.
#'
#' @section Algorithm for the difference between two screenshots:
#'
#' 1. First the two images are compared using
#'    [`testthat::compare_file_binary()`]. If the files are identical, return
#'    `TRUE` that the screenshot images are the same.
#' 2. If `threshold` is `NULL`, return `FALSE` as the convolution will not
#'    occur.
#' 3. Prepare the screenshot difference matrix by reading the RGBA channels of
#'    each image and find their respective absolute differences
#' 4. Sum the screenshot difference matrix channels at each pixel location
#' 5. Perform a convolution using a small square kernel matrix that is
#'    `kernel_size` big and filled with `1`s.
#' 6. Find the largest value in the resulting convolution matrix.
#' 7. If this max convolution value is larger than `threshold`, return `FALSE`,
#'    images are different.
#' 8. Otherwise, return `TRUE`, images are the same.
#'
#' @param old Current screenshot file path
#' @param new New screenshot file path
#' @param ... Must be empty. Allows for parameter expansion.
#' @param threshold If the value of `threshold` is `NULL`,
#' `compare_screenshot_threshold()` will act like
#' [`testthat::compare_file_binary`]. However, if `threshold` is a positive
#' number, it will be compared against the largest convolution value found if
#' the two images fail a [`testthat::compare_file_binary`] comparison. The max
#' value that can be found is `4 * kernel_size ^ 2`.
#'
#' Threshold values values below 5 help deter
#' false-positive screenshot comparisons (such as inconsistent rounded
#' corners). Larger values in the 10s and 100s will help find _real_
#' changes. However, not all values are one size fits all and you will need
#' to play with a threshold that fits your needs.
#'
#' To find the current difference between two images, use
#' `screenshot_max_difference()`.
#' @param kernel_size The `kernel_size` represents the height and width of the
#' convolution kernel applied to the matrix of pixel differences. This
#' integer-like value should be relatively small, such as 5.
#' @param quiet If `FALSE` and the value is larger than `threshold`, then a message is printed to the console. This is helpful when getting a failing image and being informed about how different the `new` image is from the `old` image.
#' @export
#' @describeIn compare_screenshot_threshold
#' Compares two images and allows for a `threshold` difference of _so many_
#' units in each RGBA color channel.
#'
#' It is suggested to use this method with
#' [`AppDriver`]`$expect_screenshot(threshold=, kernel_size=)` to make
#' expectations on screenshots given particular `threshold` and `kernel_size`
#' values.
#' @examples
#' img_folder <- system.file("example/imgs/", package = "shinytest2")
#' slider_old <- fs::path(img_folder, "slider-old.png")
#' slider_new <- fs::path(img_folder, "slider-new.png")
#'
#' # Can you see the differences between these two images?
#' showimage::show_image(slider_old)
#' showimage::show_image(slider_new)
#'
#' # You might have caught the difference between the two images!
#' slider_diff <- fs::path(img_folder, "slider-diff.png")
#' showimage::show_image(slider_diff)
#'
#' # Let's find the difference between the two images
#' screenshot_max_difference(slider_old, slider_new)
#' # ~ 28
#'
#' # Using different threshold values...
#' compare_screenshot_threshold(slider_old, slider_new, threshold = NULL)
#' #> FALSE # Images are not identical
#' compare_screenshot_threshold(slider_old, slider_new, threshold = 25)
#' #> FALSE # Images are more different than 25 units
#' compare_screenshot_threshold(slider_old, slider_new, threshold = 30)
#' #> TRUE # Images are not as different as 30 units
#'
#' #########################
#'
#' # Now let's look at two fairly similar images
#' bookmark_old <- fs::path(img_folder, "bookmark-old.png")
#' bookmark_new <- fs::path(img_folder, "bookmark-new.png")
#'
#' # Can you see the difference between these two images?
#' # (Hint: Focused corners)
#' showimage::show_image(bookmark_old)
#' showimage::show_image(bookmark_new)
#'
#' # Can you find the red pixels showing the differences?
#' # Hint: Look in the corners of the focused text
#' bookmark_diff <- fs::path(img_folder, "bookmark-diff.png")
#' showimage::show_image(bookmark_diff)
#'
#' # Let's find the difference between the two images
#' screenshot_max_difference(bookmark_old, bookmark_new)
#' # ~ 0.25
#'
#' # Using different threshold values...
#' compare_screenshot_threshold(bookmark_old, bookmark_new, threshold = NULL)
#' #> FALSE # Images are not identical
#' compare_screenshot_threshold(bookmark_old, bookmark_new, threshold = 5)
#' #> TRUE # Images are not as different than 5 units
compare_screenshot_threshold <- function(old, new, ..., threshold = NULL, kernel_size = 5, quiet = FALSE) {
  ellipsis::check_dots_empty()

  is_same_file <- testthat::compare_file_binary(old, new)
  # Quit early if they are the same file
  if (is_same_file) return(TRUE)

  # If no threshold value is provided, return the previous answer of
  # `is_same_file`
  if (is.null(threshold)) {
    return(is_same_file)
  }

  # A threshold value is provided, convolve the images

  threshold <- as.double(threshold)
  checkmate::assert_double(
    threshold,
    lower = 0,
    # Kernel size is maxed out when the full kernel is wrong on all four channels (RGBA) containing `1` values
    upper = kernel_size * kernel_size * 4,
    any.missing = FALSE,
    len = 1
  )

  conv_max_value <- screenshot_max_difference(
    old = old,
    new = new,
    kernel_size = kernel_size
  )

  ret <- conv_max_value < threshold

  if (!ret && is_false(quiet)) {
    rlang::inform(c(
      "!" = paste0("Maximum screenshot convolution value `", conv_max_value, "`",
      " > `", threshold, "` (threshold)."),
      "*" = paste0("`old`:", old),
      "*" = paste0("`new`:", new),
      "i" = crayon::silver("(To remove this message, increase `threshold`, or set `quiet = TRUE`)")
    ))
  }

  ret
}


#' @export
#' @describeIn compare_screenshot_threshold
#' Finds the difference between two screenshots.
#'
#' This value can be used in `compare_screenshot_threshold(threshold=)`. It is recommended that the value sent to `compare_screenshot_threshold(threshold=)` is larger than the difference found to tolerate allow for random rounding.
#'
#' If `new` is missing, it will use the file value of `old` (`FILE.png`) and default to `FILE.new.png`
screenshot_max_difference <- function(
  old,
  new = missing_arg(),
  ...,
  kernel_size = 5
) {
  # Use the `FILE.new.EXT`
  new <- rlang::maybe_missing(new, {
    new_ext <- fs::path_ext(old)
    new_ <- fs::path_ext_set(old, "")
    paste0(new_, ".new.", new_ext)
  })
  # Also check if exists
  checkmate::assert_file(old, extension = "png")
  checkmate::assert_file(new, extension = "png")

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

  if (any(kernel_size >= dim(new_png)[1:2])) {
    rlang::abort("`kernel_size` must be smaller than the image pixel size")
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

  # diff_matrix <- matrix(sample(as.double(1:144)), nrow = 12, ncol = 12, byrow = TRUE)

  # Use cpp11! Complexity: Theta(nrow * ncol * 2 * kernel_size)
  conv_max_value <- image_diff_convolution_max_value(
    diff_matrix,
    kernel_size = kernel_size
  )

  conv_max_value
}
