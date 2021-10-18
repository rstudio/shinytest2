
#' @include shiny-driver.R


#' @description Chromote Session object from the \pkg{chromote} package.
ShinyDriver2$set("public", "chromote_session", NULL)
#' @description Calls `$view()` on the Chromote Session object
ShinyDriver2$set("public", "view", function() {
  self$chromote_session$view()
})
