make_info_env <- function(
  suffix = missing_arg(),
  compare_images = TRUE,
  app_var = "app",
  verbose = FALSE,
  include_expect_screenshot = NULL
) {
  as.environment(list(
    suffix = suffix,
    compare_images = compare_images,
    app_var = app_var,
    include_expect_screenshot = include_expect_screenshot
  ))
}
