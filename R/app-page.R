
#' @description Get current url
#' @include shiny-driver.R
ShinyDriver2$set("public", "getUrl", function() {
  "!DEBUG ShinyDriver2$getUrl"
  chromote_eval(self$chromote_session, "window.location.href")$result$value
})

#' @description Get page title
#' @include shiny-driver.R
ShinyDriver2$set("public", "getTitle", function() {
  "!DEBUG ShinyDriver2$getTitle"
  chromote_eval(self$chromote_session, "document.title")$result$value
})

#' @description Get complete source of current page.
#' @include shiny-driver.R
ShinyDriver2$set("public", "getSource", function() {
  "!DEBUG ShinyDriver2$getSource"
  chromote_eval(self$chromote_session, "document.documentElement.outerHTML")$result$value
})

#' @description Return to previous page
#' @return Self, invisibly.
#' @include shiny-driver.R
ShinyDriver2$set("public", "goBack", function() {
  "!DEBUG ShinyDriver2$goBack"
  chromote_eval(self$chromote_session, "history.back()")$result$value
  invisible(self)
})

#' @description Refresh the browser
#' @return Self, invisibly.
#' @include shiny-driver.R
ShinyDriver2$set("public", "refresh", function() {
  # TODO-barret; Should this method be included as we can not currently shim the UI.
  #   If we can add shinytest as an htmldep, then we can use it.
  "!DEBUG refresh"
  chromote_eval(self$chromote_session, "location.reload()")$result$value
  invisible(self)
})
