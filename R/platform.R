

#' @include utils.R
# nolint start: brace_linter
is_windows <- cache_fn_val(function() { .Platform$OS.type == "windows" })
# is_mac     <- function() Sys.info()[["sysname"]] == "Darwin"
# is_linux   <- function() Sys.info()[["sysname"]] == "Linux"
# nolint end

#' Platform specific variant
#'
#' Returns a string to be used within \pkg{testthat}'s' snapshot testing. Currently, the Operating System
#' and R Version (major, minor, no patch version) are returned.
#'
#' If more information is needed in the future to distinguish standard testing environments, this function
#' will be updated accordingly.
#'
#' @param ... Must be empty. Allows for parameter expansion.
#' @param os_name if `TRUE`, include the OS name in the output
#' @param r_version if `TRUE`, include the major and minor version of the R version, no patch version
#' @seealso [`testthat::test_dir()`]
#' @export
platform_variant <- function(..., os_name = TRUE, r_version = TRUE) {
  ellipsis::check_dots_empty()

  os_name <- isTRUE(os_name)
  r_version <- isTRUE(r_version)
  if (os_name && r_version) return(paste0(os_name(), "-", r_version()))
  if (r_version) return(r_version())
  if (os_name) return(os_name())
  rlang::abort("Both `os_name` and `r_version` can not be `FALSE`")
}


os_name <- cache_fn_val(function() {
  # Inspriation and reduction of https://github.com/rstudio/renv/blob/ffe5790161fed81226577a31231299e2be2c36ba/R/platform.R
  switch(Sys.info()[["sysname"]],
    "Darwin" = "mac",
    "SunOS" = "solaris",
    "Linux" = "linux",
    .Platform$OS.type
  )
})


r_version <- cache_fn_val(function() {
  paste0(
    R.version$major,
    ".",
    strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  )
})
