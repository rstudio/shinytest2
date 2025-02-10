app_get_timeout <- function(self, private, ..., timeout = missing_arg(), timeout_name = checkmate::vname(timeout)) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  timeout <- rlang::maybe_missing(timeout, private$timeout)
  ckm8_assert_single_number(timeout, lower = 0, finite = TRUE, .var.name = timeout_name)

  timeout
}

assert_timeout <- function(timeout, timeout_name = checkmate::vname(timeout)) {
  ckm8_assert_single_number(
    timeout,
    .var.name = timeout_name,
    lower = 0,
    finite = TRUE,
    na.ok = FALSE
  )
}

timeout_env <- function(env_key) {
  env_value <- Sys.getenv(env_key, unset = "")
  if (!nzchar(env_value)) {
    # Nothing found; Return null
    return(NULL)
  }
  ret <- as.numeric(env_value)
  # Make sure the system environment variable is a number
  assert_timeout(
    ret,
    timeout_name = paste0("Sys.getenv('", env_key, "')")
  )
  ret
}

timeout_option <- function(option_key) {
  opt_value <- getOption(option_key, NULL)
  if (is.null(opt_value)) {
    # Nothing found; Return null
    return(NULL)
  }
  ret <- as.numeric(opt_value)
  # Make sure the option value is a number
  assert_timeout(
    ret,
    timeout_name = paste0("getOption('", option_key, "')")
  )
  ret
}

timeout_default <- function(default_value, timeout_name) {
  assert_timeout(
    default_value,
    timeout_name = timeout_name
  )
  default_value
}

timeout_value <- function(
  timeout,
  option_key,
  env_key,
  default_value,
  timeout_name = checkmate::vname(timeout)
) {
  # Handle both missing an NULL values
  rlang::maybe_missing(timeout, NULL) %||%       # user provided
    timeout_option(option_key) %||%              # local option
    timeout_env(env_key) %||%                    # system environment variable
    timeout_default(default_value, timeout_name) # default value
}

resolve_load_timeout <- function(load_timeout) {
  timeout_value(
    load_timeout,
    option_key = "shinytest2.load_timeout",
    env_key = "SHINYTEST2_LOAD_TIMEOUT",
    default_value = 15 * 1000,
    timeout_name = "load_timeout"
  )
}
resolve_timeout <- function(timeout) {
  timeout_value(
    timeout,
    option_key = "shinytest2.timeout",
    env_key = "SHINYTEST2_TIMEOUT",
    default_value = 4 * 1000,
    timeout_name = "timeout"
  )
}
resolve_signal_timeout <- function(signal_timeout) {
  timeout_value(
    timeout = signal_timeout,
    option_key = "shinytest2.signal_timeout",
    env_key = "SHINYTEST2_SIGNAL_TIMEOUT",
    default_value = {
      # Increased the timeout for packages like covr to upload their results:
      #   https://github.com/rstudio/shinytest2/issues/250
      # Taken from `covr::in_covr()`
      in_covr <- identical(Sys.getenv("R_COVR"), "true")
      if (in_covr) {
        20 * 1000
      } else {
        500
      }
    }
  )
}


app_init_timeouts <- function(
  self, private,
  ...,
  load_timeout = missing_arg(),
  timeout = missing_arg()
) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  private$load_timeout <- resolve_load_timeout(load_timeout)

  private$timeout <- resolve_timeout(timeout)
}
