app_get_timeout <- function(self, private, ..., steps = 1) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  ckm8_assert_single_number(steps, lower = 0, finite = TRUE)

  steps * private$timeout_stepsize
}

app_set_timeout <- function(self, private, ..., stepsize = 1000) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  ckm8_assert_single_number(stepsize, lower = 0, finite = TRUE)

  private$timeout_stepsize <- stepsize
  invisible(self)
}


app_init_timeout <- function(self, private, ..., stepsize = missing_arg()) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  # Allow timeout to be larger if running on CI
  stepsize <- rlang::maybe_missing(stepsize, {
    getOption("shinytest2.timeout.stepsize", {
      if (on_ci()) {
        2 * 1000
      } else {
        1 * 1000
      }
    })
  })

  self$set_timeout(stepsize = stepsize)
}
