app_execute_script <- function(
  self, private,
  script,
  arguments = list(),
  ...,
  timeout = 15 * 1000
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  # TODO-barret; incorporate `wait_` parameters to not wait for the _tick_ to finish?
  # TODO-barret; Maybe they should make a promise and return NULL instead?
  "!DEBUG app_execute_script()"
  chromote_execute_script(
    self$get_chromote_session(),
    script,
    awaitPromise = TRUE,
    arguments = arguments,
    timeout = timeout
  )$result$value
}

app_execute_script_callback <- function(
  self, private,
  script,
  arguments = list(),
  ...,
  timeout = 15 * 1000
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  # TODO-barret; incorporate `wait_` parameters to not wait for the _tick_ to finish
  # TODO-barret; Maybe they should make a promise and return NULL instead?
  "!DEBUG app_execute_script_callback()"
  chromote_execute_script_callback(
    self$get_chromote_session(),
    script,
    arguments = arguments,
    timeout = timeout
  )
  invisible(self)
}
