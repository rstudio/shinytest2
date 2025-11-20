skip_if_no_local_apps()
skip_on_cran()

# Use test_app to relay the tests to the top level testthat runner
test_app(test_path("local-apps/html-text"), quiet = TRUE)
test_app(test_path("local-apps/window"), quiet = TRUE)
