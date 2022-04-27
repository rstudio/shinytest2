skip_on_cran() # Uses chromote


dirs <- dir(test_path("apps"), full.names = TRUE)
lapply(dirs, function(shiny_app_dir) {
  test_path <- file.path(shiny_app_dir, "tests", "")
  if (dir.exists(test_path)) {

    rlang::inform(shiny_app_dir)
    test_that(paste0("All apps pass their tests - ", shiny_app_dir), {
      # Test that `test_app()` is performing an expectation in a testing setting
      # expect_success({
        # Given only testthat tests are used
        test_app(shiny_app_dir)
      # })

      expect_equal(TRUE, TRUE)

      # # If non-testthat tests are used, this code should be used instead:
      # expect_error(
      #   shiny::runTests(shiny_app_dir, assert = TRUE),
      #   NA,
      #   info = shiny_app_dir
      # )
    })

  }
})
