rstudio_is_available <- function() {
  rlang::check_installed("rstudioapi")
  rstudioapi::isAvailable()
}
