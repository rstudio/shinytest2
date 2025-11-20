app_dir <- test_path("local-apps/window")
if (!dir.exists(file.path(app_dir, "tests", "testthat"))) {
  skip("Test local-apps folders have been ignored")
}

skip_on_cran()

# Use test_app to relay the tests to the top level testthat runner
test_app(test_path("local-apps/html-text"), quiet = TRUE)
test_app(test_path("local-apps/window"), quiet = TRUE)
