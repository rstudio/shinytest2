
#' @description
#' Returns a named list of all inputs, outputs, and export values.
#'
#' @param input,output,export Either `TRUE` to return all
#'   input/output/exported values, or a character vector of specific
#'   controls.
#' @include shiny-driver.R
ShinyDriver2$set("public", "getAllValues", function(input = TRUE, output = TRUE, export = TRUE) {
  # Note: This queries the server
  self$logEvent("Getting all values")
  "!DEBUG ShinyDriver2$getAllValues"

  message("ShinyDriver2$getAllValues()")
  url <- private$getTestSnapshotUrl(input, output, export, format = "rds")
  req <- httr_get(url)

  tmpfile <- tempfile()
  on.exit(unlink(tmpfile))
  writeBin(req$content, tmpfile)
  readRDS(tmpfile)
})
