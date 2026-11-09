app_warn <- function(self, private, ..., .envir = rlang::caller_env()) {
  ckm8_assert_app_driver(self, private)
  cli::cli_warn(..., app = self, .envir = .envir)
}
app_inform <- function(self, private, ...) {
  ckm8_assert_app_driver(self, private)
  rlang::inform(..., app = self)
}
app_abort <- function(
  self,
  private,
  ...,
  call = rlang::caller_env(),
  .envir = rlang::caller_env()
) {
  ckm8_assert_app_driver(self, private)
  cli::cli_abort(..., app = self, call = call, .envir = .envir)
}

# Escape `{` and `}` so text is shown as-is by cli
cli_escape <- function(x) {
  gsub("([{}])", "\\1\\1", x)
}
# Split text into escaped, named lines so cli keeps the line breaks
cli_lines <- function(x, name = " ") {
  x <- cli_escape(unlist(strsplit(
    paste0(x, collapse = "\n"),
    "\n",
    fixed = TRUE
  )))
  rlang::set_names(x, rep(name, length(x)))
}
