app_set_window_size <- function(self, private, width, height, wait = TRUE) {
  "!DEBUG app_set_window_size() `width`x`height`"
  ckm8_assert_app_driver(self, private)

  self$log_message(paste0(
    "Setting window size to `", width, "`x`", height, "`"
  ))

  chromote_set_window_size(self$get_chromote_session(), width = width, height = height)

  if (isTRUE(wait)) {
    self$wait_for_idle()
  }
  # # Support this? No. Can manually call `app$wait_for_idle()` themselves
  # else if (is.list(wait)) {
  #   rlang::eval_tidy(self$wait_for_idle(!!!wait))
  # }
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
