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
  delay = 0,
  selector = "html",
  cliprect = NULL,
  region = c("content", "padding", "border", "margin"),
  expand = NULL,
  scale = 1,
  wait_ = TRUE
) {
  "!DEBUG sd2_takeScreenshot"
  stopifnot(isTRUE(wait_))
  delay <- delay %||% 0
  checkmate::assert_number(delay, lower = 0, finite = TRUE, null.ok = TRUE)

  self$logEvent("Taking screenshot")
  path <- tempfile("st2-", fileext = ".png")

  # # TODO-barret: implement `selector` usage. May have to go back to using `chromote_obj$screenshot()`
  # if (delay > 0) {
  #   Sys.sleep(delay)
  # }
  # screenshot_data <- self$chromote_session$Page$captureScreenshot(format = "png")$data
  # writeBin(
  #   jsonlite::base64_dec(screenshot_data),
  #   path
  # )
  # TODO-barret: use prior screenshot code below instead of code above. The code below works with selectors and larger regions.
  self$chromote_session$screenshot(
    filename = path,
    ...,
    delay = delay,
    selector = selector,
    cliprect = cliprect,
    region = region,
    expand = expand,
    scale = scale,
    wait_ = wait_
  )

  # Fix up the PNG resolution header on windows
  if (is_windows()) {
    normalize_png_res_header(path)
  }

  if (!is.null(list2(...)$id)) {
    stop("TODO-barret; Remove? This should be able to be implemented using `#ID` selector")
    png <- png::readPNG(path)
    element <- self$findElement(paste0("#", id))
    if (parent) {
      element <- element$findElement(xpath = "..")
    }
    pos <- element$getRect()
    pos$x2 <- pos$x + pos$width
    pos$y2 <- pos$y + pos$height

    png <- png[pos$y:pos$y2, pos$x:pos$x2, ]
    png::writePNG(png, path)
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
