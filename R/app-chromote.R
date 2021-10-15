
#' @include shiny-driver.R
ShinyDriver2$set("private", "chromote_obj", NULL) # chromote session object
# TODO-barret; make regular public field
# TODO-barret; rename to `chromote_session`
# chromote_session = NULL,
ShinyDriver2$set("active", "chromote_session", function(value) {
  if (!missing(value)) {
    stop("`$chromote` can only be read, not set")
  }
  private$chromote_obj
})
