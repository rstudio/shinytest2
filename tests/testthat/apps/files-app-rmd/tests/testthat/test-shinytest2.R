library(shinytest2)

test_that("rmarkdown and app.R are not compatible", {
  expect_error(
    AppDriver$new(),
    "`app_dir` must be a directory containing",
    fixed = TRUE
  )
})
