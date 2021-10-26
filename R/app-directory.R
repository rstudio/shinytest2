#' @include shiny-driver.R
ShinyDriver2$set("private", "path", NULL) # Full path to app (including filename if it's a .Rmd)


# TODO-barret-issue; Can we get rid of $getAppFilename, $getAppDir, and $isRmd?
# TODO-barret-issue; Why not make `private$path` into `public$path`? https://github.com/rstudio/shinytest2/issues/43

#' @description Directory where app is located
#' @include shiny-driver.R
ShinyDriver2$set("public", "getAppDir", function() {
  # path can be a directory (for a normal Shiny app) or path to a .Rmd
  if (self$isRmd()) fs::path_dir(private$path) else private$path
})

#' @description The Rmd filename or NULL
#' @include shiny-driver.R
ShinyDriver2$set("public", "getAppFilename", function() {
  if (self$isRmd()) {
    fs::path_file(private$path)
  } else {
    NULL
  }
})

#' @description Is this app an Shiny Rmd document?
#' @include shiny-driver.R
ShinyDriver2$set("public", "isRmd", function() {
  is_rmd(private$path)
})
