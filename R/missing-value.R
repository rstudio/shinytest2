# Note: `missing_value()` is used as it can be printed by `R6`, where as `rlang::missing_arg()` can not be printed.
# If any `AppDriver` objects need to hold onto a `rlang::missing_arg()`, `missing_value()` should be used instead.

# Basic missing value structure
missing_value <- function() {
  structure(list(), class = "shinytest2_missing_value")
}
# Handle rlang::missing_arg() values too!
is_missing_value <- function(value) {
  rlang::is_missing(value) || inherits(value, "shinytest2_missing_value")
}

# Mimic `rlang::maybe_missing()`
# Handle rlang::missing_arg() values too!
maybe_missing_value <- function(value, default = missing_value()) {
  if (is_missing_value(value)) {
    default
  } else {
    value
  }
}
