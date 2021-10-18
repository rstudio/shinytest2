#' @description Execute JS code
#' @param script JS to execute. If a JS Promise is returned, `$executeScript()` will wait for the promise to resolve before returning.
#' @param ... Additional arguments to script.
#' @return Result of the script.
#' @include shiny-driver.R
ShinyDriver2$set("public", "executeScript", function(script, ...) {
  # TODO-barret; chromote$Runtime$evaluate() is blocking for the JS _tick_
  # TODO-barret; incorporate `wait_` parameters to not wait for the _tick_ to finish
  "!DEBUG ShinyDriver2$executeScript"
  chromote_execute_script(
    private$chromote_obj,
    script,
    awaitPromise = TRUE,
    arguments = rlang::list2(...)
  )$result$value
})

#' @description Execute JS code asynchronously.
#' @param script JS to execute. If a JS Promise is returned, `$executeScriptAsync()` will wait for the Promise to be resolved.
#' @param ... Additional arguments to script.
#' @return Self, invisibly.
#' @include shiny-driver.R
ShinyDriver2$set("public", "executeScriptCallback", function(script, ..., timeout_ = 15 * 1000) {
  "!DEBUG ShinyDriver2$executeScriptCallback"
  chromote_execute_script_callback(
    private$chromote_obj,
    script,
    arguments = rlang::list2(...)
  )
  invisible(self)
})
