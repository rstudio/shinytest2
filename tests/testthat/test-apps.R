
dirs <- dir(test_path("apps"), full.names = TRUE)
lapply(dirs, function(shiny_app_dir) {
  if (dir.exists(file.path(shiny_app_dir, "tests"))) {

    # message(shiny_app_dir)
    test_that(paste0("All apps pass their tests - ", shiny_app_dir), {
      expect_error(
        shiny::runTests(shiny_app_dir, assert = TRUE),
        NA
      )
    })

  }
})
