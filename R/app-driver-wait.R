

app_wait_for_condition <- function(
  self, private,
  expr,
  timeout = 3 * 1000,
  interval = 100
) {
  "!DEBUG app_wait_for_condition()"
  ckm8_assert_app_driver(self, private)

  chromote_wait_for_condition(
    self$get_chromote_session(),
    expr,
    timeout = timeout,
    interval = interval
  )
}

# TODO-barret-implement; Add `self$wait_for_stable()`; Remove `self$waitForIdle()`?
app_wait_for_idle <- function(self, private, timeout = 3 * 1000, interval = 100) {
  ckm8_assert_app_driver(self, private)

  # Shiny automatically sets using busy/idle events:
  # https://github.com/rstudio/shiny/blob/e2537d/srcjs/shinyapp.js#L647-L655
  # Details of busy event: https://shiny.rstudio.com/articles/js-events.html
  # private$web$waitFor()
  chromote_wait_for_condition(
    self$get_chromote_session(),
    "!$('html').first().hasClass('shiny-busy')",
    timeout = 3 * 1000,
    interval = interval
  )
}

app_wait_for_value <- function(
  self, private,
  id,
  ignore = list(NULL, ""),
  iotype = c("input", "output", "export"),
  timeout = 10 * 1000,
  interval = 400
) {
  "!DEBUG app_wait_for_value()"
  ckm8_assert_app_driver(self, private)

  iotype <- match.arg(iotype)

  checkmate::assert_number(timeout, lower = 0, finite = FALSE, na.ok = FALSE)
  checkmate::assert_number(interval, lower = 0, finite = FALSE, na.ok = FALSE)

  timeoute_sec <- timeout / 1000
  interval_sec <- interval / 1000

  now <- function() {
    as.numeric(Sys.time())
  }

  end_time <- now() + timeoute_sec

  # by default, do not retrieve anything
  input <- output <- export <- FALSE
  # update the correct value, given the iotype
  switch(iotype,
    "input"  = {
      input <- id
    },
    "output" = {
      output <- id
    },
    "export" = {
      export <- id
    }
  )

  while (TRUE) {
    value <- try({
      self$get_values(input = input, output = output, export = export)
    }, silent = TRUE)

    # if no error when trying to retrieve the value..
    if (!inherits(value, "try-error")) {
      # check against all invalid values
      is_invalid <- vapply(ignore, identical, logical(1), x = value)
      # if no matches, then it's a success!
      if (!any(is_invalid)) {
        return(value)
      }
    }

    # if too much time has elapsed... throw
    if (now() > end_time) {
      abort(paste0("timeout reached when waiting for value: ", id))
    }

    # wait a little bit for shiny to do some work
    Sys.sleep(interval_sec)
  }
  # never reached
}
