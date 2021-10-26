#' @include shiny-driver.R
ShinyDriver2$set("private", "path", NULL) # Full path to app (including filename if it's a .Rmd)
ShinyDriver2$set("public", "getPath", function() {
  private$path
})
