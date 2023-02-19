# testthat on_cran
on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

# Check: for detritus in the temp directory
# Result: NOTE
#     Found the following files/directories:
#      ‘Crashpad’
# Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc
if (on_cran()) {
  test_that("Make sure `CrashPad` is cleaned up", {
    pkg_dir <- system.file(package = "shinytest2")
    if (pkg_dir == "") {
      skip("shinytest2 not installed")
    }
    crashpad_path <- file.path(pkg_dir, "Crashpad")
    unlink(crashpad_path, recursive = TRUE)

    # Make an expectation
    expect_true(TRUE)
  })
}
