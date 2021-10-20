# Directory for temp storing test artifacts

#' @description
#' Takes a screenshot of the current page and writes it to a PNG file or
#' shows on current graphics device.
#' @param file File name to save the screenshot to. If `NULL`, then
#'   it will be shown on the R graphics device.
#' @param id If not-`NULL`, will take a screenshot of element with this id.
#' @param parent If `TRUE`, will take screenshot of parent of `id`; this
#'   is useful if you also want to capture the label attached to a Shiny
#'   control.
#' @param delay The amount of seconds to wait before taking a screenshot
#' @return Self, invisibly.
#' @include shiny-driver.R
# takeScreenshotLegacy = function(file = NULL, id = NULL, parent = FALSE) {
ShinyDriver2$set("public", "takeScreenshot", function(
  filename = NULL,
  ..., # ignored? Send to chromote?
  # TODO-barret; Are all of these params needed? "Less is more"
  screenshot_args = list(), # TODO-barret; Use this instead of `...` / extra args?
  delay = 0,
  selector = "html",
  wait_ = TRUE
) {
  "!DEBUG sd2_takeScreenshot"
  stopifnot(isTRUE(wait_))
  delay <- delay %||% 0
  checkmate::assert_number(delay, lower = 0, finite = TRUE, null.ok = TRUE)

  self$logEvent("Taking screenshot")
  path <- temp_file(".png")

  self$chromote_session$screenshot(
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
})
