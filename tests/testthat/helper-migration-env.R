make_info_env <- function(
  suffix = missing_arg(),
  compare_images = TRUE,
  app_var = "app",
  verbose = FALSE
) {
  as.environment(list(
    suffix = suffix,
    compare_images = compare_images,
    app_var = app_var
  ))
}
