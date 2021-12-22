

app_wait_for_script <- function(
  self, private,
  script,
  timeout = 3 * 1000,
  interval = 100
) {
  "!DEBUG app_wait_for_script()"
  ckm8_assert_app_driver(self, private)

  # Will throw error if timeout is exceeded
  chromote_wait_for_condition(
    self$get_chromote_session(),
    script,
    timeout = timeout,
    interval = interval
  )
  invisible(self)
}

app_wait_for_stable <- function(self, private, duration = 500, timeout = 3 * 1000) {
  ckm8_assert_app_driver(self, private)

  checkmate::assert_number(duration, lower = 0, finite = TRUE)
  checkmate::assert_number(timeout, lower = 0, finite = TRUE)

  stable_js <- "
  let duration = arguments[0]; // time needed to be idle
  let timeout = arguments[1]; // max total time

  return new Promise((resolve, reject) => {

    window.shinytest2.log('Waiting for Shiny to be stable');

    const cleanup = () => {
      $(document).off('shiny:busy', busyFn);
      $(document).off('shiny:idle', idleFn);
      clearTimeout(timeoutId);
      clearTimeout(idleId);
    }

    let timeoutId = setTimeout(() => {
      cleanup();
      reject('Shiny did not become stable within ' + timeout + 'ms');
    }, +timeout); // make sure timeout is number

    let idleId = null;
    const busyFn = () => {
      // clear timeout. Calling with `null` is ok.
      clearTimeout(idleId);
    };
    const idleFn = () => {
      const fn = () => {
        // Made it through the required duration
        // Remove event listeners
        cleanup();
        window.shinytest2.log('Shiny has been idle for ' + duration + 'ms');
        // Resolve the promise
        resolve();
      };

      // delay the callback wrapper function
      idleId = setTimeout(fn, +duration);
    };

    // set up individual listeners for this function.
    $(document).on('shiny:busy', busyFn);
    $(document).on('shiny:idle', idleFn);

    // if already idle, call `idleFn` to kick things off.
    if (window.shinytest2.busy !== true) {
      idleFn();
    }
  })
  "

  ret <- chromote_execute_script(
    self$get_chromote_session(),
    stable_js,
    arguments = list(
      duration,
      timeout
    ),
    ## Supply a large "wall time" to chrome devtools protocol. The manual logic should be hit first
    timeout = timeout * 2
  )

  if (identical(ret$result$subtype, "error") || length(ret$exceptionDetails) > 0) {
    abort("An error occurred while waiting for Shiny to be stable")
  }

  invisible(self)
}

app_wait_for_value <- function(
  self, private,
  input = missing_arg(),
  output = missing_arg(),
  export = missing_arg(),
  ...,
  ignore = list(NULL, ""),
  timeout = 10 * 1000,
  interval = 400
) {
  "!DEBUG app_wait_for_value()"
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  checkmate::assert_number(timeout, lower = 0, finite = FALSE, na.ok = FALSE)
  checkmate::assert_number(interval, lower = 0, finite = FALSE, na.ok = FALSE)

  timeoute_sec <- timeout / 1000
  interval_sec <- interval / 1000

  now <- function() {
    as.numeric(Sys.time())
  }

  end_time <- now() + timeoute_sec

  input_provided <- !rlang::is_missing(input)
  output_provided <- !rlang::is_missing(output)
  export_provided <- !rlang::is_missing(export)
  if (sum(input_provided, output_provided, export_provided) != 1) {
    abort("You must specify either `input`, `output`, or `export`")
  }
  if (input_provided) ckm8_assert_single_string(input)
  if (output_provided) ckm8_assert_single_string(output)
  if (export_provided) ckm8_assert_single_string(export)
  # by default, do not retrieve anything
  input <- rlang::maybe_missing(input, FALSE)
  output <- rlang::maybe_missing(output, FALSE)
  export <- rlang::maybe_missing(export, FALSE)

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
