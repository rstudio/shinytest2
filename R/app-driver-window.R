app_set_window_size <- function(self, private, width, height) {
  "!DEBUG app_set_window_size() `width`x`height`"
  ckm8_assert_app_driver(self, private)

  chromote_set_window_size(self$get_chromote_session(), width = width, height = height)
  invisible(self)
}

app_get_window_size <- function(self, private) {
  "!DEBUG app_get_window_size()"
  ckm8_assert_app_driver(self, private)

  viewport <- self$get_chromote_session()$Page$getLayoutMetrics()$cssLayoutViewport
  list(
    width = viewport$clientWidth,
    height = viewport$clientHeight
  )
}
