#' @description
#' Sets size of the browser window.
#' @param width,height Height and width of browser, in pixels.
#' @return Self, invisibly.
#' @include shiny-driver.R
ShinyDriver2$set("public", "setWindowSize", function(width, height) {
  "!DEBUG ShinyDriver2$setWindowSize `width`x`height`"
  chromote_set_window_size(self$get_chromote_session(), width = width, height = height)
  invisible(self)
})

#' @description
#' Get current size of the browser window, as list of integer scalars
#'   named `width` and `height`.
#' @include shiny-driver.R
ShinyDriver2$set("public", "getWindowSize", function() {
  "!DEBUG ShinyDriver2$getWindowSize"
  viewport <- self$get_chromote_session()$Page$getLayoutMetrics()$cssLayoutViewport
  list(
    width = viewport$clientWidth,
    height = viewport$clientHeight
  )
})
