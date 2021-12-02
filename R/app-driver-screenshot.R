app_screenshot <- function(
  self, private,
  filename = NULL,
  ...,
  # TODO-barret-question; Are all of these params needed? "Less is more"
  screenshot_args = list(), # TODO-barret-answer; Use this instead of `...` / extra args?
  delay = 0,
  selector = "html",
  wait_ = TRUE
) {
  "!DEBUG app_screenshot"
  ckm8_assert_app_driver(self, private)

  stopifnot(isTRUE(wait_))
  delay <- delay %||% 0
  checkmate::assert_number(delay, lower = 0, finite = TRUE, null.ok = TRUE)

  self$log_event("Taking screenshot")
  path <- temp_file(".png")

  self$get_chromote_session()$screenshot(
    filename = path,
    ...,
    delay = delay,
    selector = selector,
    wait_ = wait_
  )

  # Fix up the PNG resolution header on windows
  if (is_windows()) {
    normalize_png_res_header(path)
  }

  if (is.null(filename)) {
    withr::local_par(list(bg = "grey90"))
    png <- png::readPNG(path)
    plot(as.raster(png))
  } else {
    # utils::str(list(
    #   path = path,
    #   file = filename
    # ))
    fs::file_copy(path, filename)
  }

  invisible(self)
}
