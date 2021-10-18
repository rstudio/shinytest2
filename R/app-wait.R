

#' @description
#' Waits until a JavaScript `expr`ession evaluates to `true` or the
#' `timeout` is exceeded.
#' @param expr A string containing JavaScript code. Will wait until the
#'   condition returns `true`.
#' @param check_interval How often to check for the condition, in ms.
#' @return `TRUE` if expression evaluates to `true` without error, before
#'   timeout. Otherwise returns `FALSE`.
#' @include shiny-driver.R
ShinyDriver2$set("public", "waitFor", function(expr, check_interval = 100, timeout = 3000) {
  "!DEBUG ShinyDriver2$waitFor"
  chromote_wait_for_condition(private$chromote_obj, expr, timeout_ms = timeout, delay_ms = check_interval)
})

#' @description
#' Waits until Shiny is not busy, i.e. the reactive graph has finished
#' updating. This is useful, for example, if you've resized the window with
#' `setWindowSize()` and want to make sure all plot redrawing is complete
#' before take a screenshot.
#' @return `TRUE` if done before before timeout; `NA` otherwise.
#' @include shiny-driver.R
ShinyDriver2$set("public", "waitForShiny", function()  {
  # Shiny automatically sets using busy/idle events:
  # https://github.com/rstudio/shiny/blob/e2537d/srcjs/shinyapp.js#L647-L655
  # Details of busy event: https://shiny.rstudio.com/articles/js-events.html
  # private$web$waitFor()
  chromote_wait_for_condition(
    private$chromote_obj,
    "!$('html').first().hasClass('shiny-busy')",
    timeout = 3 * 1000,
    delay_ms = check_interval
  )
})

#' @description
#' Waits until the `input` or `output` with name `name` is not one of
#' `ignore`d values, or the timeout is reached.
#'
#' This function can be useful in helping determine if an application
#' has initialized or finished processing a complex reactive situation.
#' @param ignore List of possible values to ignore when checking for
#'   updates.
#' @include shiny-driver.R
ShinyDriver2$set("public", "waitForValue", function(
  name,
  ignore = list(NULL, ""),
  iotype = c("input", "output", "export"),
  timeout = 10000,
  check_interval = 400
) {
  "!DEBUG ShinyDriver2$waitForValue"

  iotype <- match.arg(iotype)

  checkmate::assert_number(timeout, lower = 0, finite = FALSE, na.ok = FALSE)
  checkmate::assert_number(check_interval, lower = 0, finite = FALSE, na.ok = FALSE)

  timeoute_sec <- timeout / 1000
  check_interval_sec <- check_interval / 1000

  now <- function() {
    as.numeric(Sys.time())
  }

  end_time <- now() + timeoute_sec

  while (TRUE) {
    value <- try({
      # by default, do not retrieve anything
      args <- list(input = FALSE, output = FALSE, export = FALSE)
      # only retrieve `name` from `iotype`
      args[[iotype]] <- name
      # TODO(-prev); Should this be `self$getValue(name, iotype = iotype)`? Note: `self$getValue()` is not generic enough?
      do.call(self$getAllValues, args)[[iotype]][[name]]
    }, silent = TRUE)

    # if no error when trying ot retrieve the value..
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
      abort(paste0("timeout reached when waiting for value: ", name))
    }

    # wait a little bit for shiny to do some work
    Sys.sleep(check_interval_sec)
  }

  invisible(self)
})
