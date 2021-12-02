app_get_chromote_session <- function(self, private) {
  ckm8_assert_app_driver(self, private)

  private$chromote_session
}

app_view <- function(self, private) {
  ckm8_assert_app_driver(self, private)

  self$get_chromote_session()$view()
}
