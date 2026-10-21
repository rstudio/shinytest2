#' Safely get the path to the package in development, if any
#' @noRd
dev_pkg_path <- function() {
  path <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)
  # Installed packages also have a DESCRIPTION file (e.g. tests run via
  # `testthat::test_package()`), but only installed packages have `Meta/`
  if (!is.null(path) && file.exists(file.path(path, "Meta", "package.rds"))) {
    return(NULL)
  }
  path
}

#' Safely get the name of the package in development, if any
#' @noRd
dev_pkg_name <- function() {
  path <- dev_pkg_path()
  if (is.null(path)) {
    return(NULL)
  }
  tryCatch(pkgload::pkg_name(path), error = function(e) NULL)
}

#' Get the namespace of the package in development, if any
#' @noRd
dev_pkg_ns <- function() {
  pkgload::pkg_ns()
}

in_dev_pkg <- function(pkg_path = dev_pkg_path()) {
  !is.null(pkg_path) && getwd() == pkg_path
}
