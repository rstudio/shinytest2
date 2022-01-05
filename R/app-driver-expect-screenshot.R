app_screenshot <- function(
  self, private,
  filename = NULL,
  ...,
  screenshot_args = list(),
  delay = screenshot_args$delay %||% 0,
  selector = screenshot_args$selector %||% "html"
) {
  "!DEBUG app_screenshot()"
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  if (is.null(screenshot_args) || isTRUE(screenshot_args)) screenshot_args <- list()
  checkmate::assert_list(screenshot_args)

  screenshot_args$delay <- delay %||% 0
  screenshot_args$selector <- selector %||% "html"

  checkmate::assert_number(screenshot_args$delay, lower = 0, finite = TRUE, null.ok = TRUE)

  if (is.null(filename)) {
    self$log_message("Taking screenshot")
  } else {
    self$log_message(paste0("Taking screenshot: ", filename))
  }
  path <- temp_file(".png")
  screenshot_args$filename <- path

  do.call(self$get_chromote_session()$screenshot, screenshot_args)

  # Fix up the PNG resolution header on windows
  if (is_windows()) {
    normalize_png_res_header(path)
  }

  if (is.null(filename)) {
    withr::local_par(list(bg = "grey90"))
    png <- png::readPNG(path)
    graphics::plot(grDevices::as.raster(png))
  } else {
    fs::file_copy(path, filename)
  }

  invisible(self)
}