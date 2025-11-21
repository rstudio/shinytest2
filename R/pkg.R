#' Safely get the path to the package in development, if any
#' @noRd
dev_pkg_path <- function() {
  tryCatch(pkgload::pkg_path(), error = function(e) NULL)
}

#' Safely get the name of the package in development, if any
#' @noRd
dev_pkg_name <- function() {
  tryCatch(pkgload::pkg_name(), error = function(e) NULL)
}

#' Get the namespace of the package in development, if any
#' @noRd
dev_pkg_ns <- function() {
  pkgload::pkg_ns()
}

in_dev_pkg <- function(pkg_path = dev_pkg_path()) {
  !is.null(pkg_path) && getwd() == pkg_path
}
