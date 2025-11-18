pkg_dir <- test_path("pkgs/expkg")
if (!dir.exists(file.path(pkg_dir, "tests", "testthat"))) {
  skip("Test packages folders have been ignored")
}

skip_on_cran()


# Use test_app to relay the tests to the top level testthat runner
# It is not testing an _app_ per se, but this allows us to
# test the package loading behavior within the shinytest2 test suite easily
test_app(test_path("pkgs/expkg"), load_package = "source", check_setup = FALSE)

if (!requireNamespace("golem", quietly = TRUE)) {
  testthat::test_that(paste0("pkgs - golem"), {
    skip("golem not installed")
  })
} else {
  test_app(
    test_path("pkgs/golem"),
    load_package = "source",
    check_setup = FALSE
  )
}

if (
  !(requireNamespace("rhino", quietly = TRUE) &&
    requireNamespace("box", quietly = TRUE))
) {
  testthat::test_that(paste0("pkgs - rhino"), {
    skip("rhino not installed")
  })
} else {
  test_app(
    test_path("pkgs/rhino"),
    load_package = "none",
    check_setup = FALSE
  )
}
