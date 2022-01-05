app_get_values <- function(
  self, private,
  input = TRUE,
  output = TRUE,
  export = TRUE
) {
  ckm8_assert_app_driver(self, private)
  # Note: This queries method the Shiny server

  self$log_message("Getting all values")
  "!DEBUG app_get_values()"

  url <- app_get_shiny_test_url(self, private, input, output, export, format = "rds")
  req <- httr_get(url)

  tmpfile <- temp_file(".rds")
  on.exit(unlink(tmpfile))
  writeBin(req$content, tmpfile)
  readRDS(tmpfile)
}
