#' @description
#' Sets size of the browser window.
#' @param width,height Height and width of browser, in pixels.
#' @return Self, invisibly.
#' @include shiny-driver.R
ShinyDriver2$set("public", "set_window_size", function(width, height) {
  "!DEBUG ShinyDriver2$set_window_size `width`x`height`"
  chromote_set_window_size(self$get_chromote_session(), width = width, height = height)
  invisible(self)
})

#' @description
#' Get current size of the browser window, as list of integer scalars
#'   named `width` and `height`.
#' @include shiny-driver.R
ShinyDriver2$set("public", "get_window_size", function() {
  "!DEBUG ShinyDriver2$get_window_size"
  viewport <- self$get_chromote_session()$Page$getLayoutMetrics()$cssLayoutViewport
  list(
    width = viewport$clientWidth,
    height = viewport$clientHeight
  )
})
