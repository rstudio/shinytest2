
#' @include shiny-driver.R


#' @description Chromote Session object from the \pkg{chromote} package.
ShinyDriver2$set("private", "chromote_session", NULL)
ShinyDriver2$set("public", "get_chromote_session", function() {
  private$chromote_session
})
## TODO-barret; implement?
# #' @description Calls `$view()` on the Chromote Session object
# ShinyDriver2$set("public", "view", function() {
#   self$get_chromote_session()$view()
# })
