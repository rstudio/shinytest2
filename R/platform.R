

is_windows <- cache_fn_val(function() { .Platform$OS.type == "windows" })
# is_mac     <- function() Sys.info()[["sysname"]] == "Darwin"
# is_linux   <- function() Sys.info()[["sysname"]] == "Linux"

#' Operating System and R version values
#'
#' Returns the name of the current OS. This can be useful for the `variant` when
#' running [`testthat::test_dir()`].
#'
#' @export
#' @describeIn os_name Operating system name
#' @include utils.R
os_name <- cache_fn_val(function() {
  # Inspriation and reduction of https://github.com/rstudio/renv/blob/ffe5790161fed81226577a31231299e2be2c36ba/R/platform.R
  switch(Sys.info()[["sysname"]],
    "Darwin" = "mac",
    "SunOS" = "solaris",
    "Linux" = "linux",
    {
      switch(.Platform$OS.type,
        "windows" = "windows",
        "unix" = "unix",
        stop("Unknown OS")
      )
    }
  )
})

#' @export
#' @describeIn os_name Return the Major and Minor version of the OS. No Patch version
r_version <- cache_fn_val(function() {
  paste0(
    R.version$major,
    ".",
    strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  )
})


#' @export
#' @describeIn os_name Return the `os_name()` and `r_version()` join by a `-`
os_name_and_r_version <- cache_fn_val(function() {
  paste0(
    os_name(),
    "-",
    r_version()
  )
})
