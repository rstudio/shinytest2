#' @include shiny-driver.R
ShinyDriver2$set("private", "path", NULL) # Full path to app (including filename if it's a .Rmd)


#' @description Directory where app is located
#' @include shiny-driver.R
ShinyDriver2$set("public", "getAppDir", function() {
  # path can be a directory (for a normal Shiny app) or path to a .Rmd
  if (self$isRmd()) fs::path_dir(private$path) else private$path
})

#' @description App file name, i.e. `app.R` or `server.R`. `NULL` for Rmds.
#' @include shiny-driver.R
ShinyDriver2$set("public", "getAppFilename", function() {
  if (!self$isRmd()) {
    NULL
  } else {
    fs::path_file(private$path)
  }
})

#' @description Is this app an Shiny Rmd document?
#' @include shiny-driver.R
ShinyDriver2$set("public", "isRmd", function() {
  is_rmd(private$path)
})

# #' @description Deprecated.
# #' @include shiny-driver.R
# ShinyDriver2$set("public", "getRelativePathToApp", function() {
#   abort("app$getRelativePathToApp() is no longer used")
# })

# #' @description Deprecated. Directory where tests are located
# #' @include shiny-driver.R
# ShinyDriver2$set("public", "getTestsDir", function() {
#   abort("app$getTestsDir() is not longer used")

#   # From the 'app' to the 'tests/testthat' directory
#   rprojroot::find_testthat_root_file(path = self$getAppDir())
# })
