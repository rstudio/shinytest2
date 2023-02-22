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

  # Disable crash reporting on CRAN machines. (Can't get the report anyways)
  chromote::set_chrome_args(c(
    # https://peter.sh/experiments/chromium-command-line-switches/#disable-crash-reporter
    #> Disable crash reporter for headless. It is enabled by default in official builds
    "--disable-crash-reporter",
    chromote::default_chrome_args()
  ))

  # Make sure the temp folder is removed when testing is complete
  withr::defer({
    # Close the browser
    try(chromote::default_chromote_object()$get_browser()$close())

    # Clean up chromote sessions
    gc() # Run R6 finalizer methods
    Sys.sleep(2) # Wait for any supervisors to exit

    # Delete the Crashpad folder if it exists
    unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
  }, envir = testthat::teardown_env())
}
