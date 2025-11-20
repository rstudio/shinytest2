test_that("Spell check", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("spelling")

  spelling_errors <- spelling::spell_check_package(test_path("../../"))
  if (nrow(spelling_errors) > 0) {
    res_print <-
      paste0(
        capture.output({
          print(as.data.frame(spelling_errors))
        }),
        collapse = "\n"
      )
  } else {
    res_print <- NULL
  }
  expect_equal(nrow(spelling_errors), 0, info = res_print)
})
