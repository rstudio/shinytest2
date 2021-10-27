#' @description Execute JS code
#'
#' This function will block until the code has finished executing its _tick_ in the browser.
#' If a `Promise` is returned from the script, `$executeScript()` will wait for the promise to resolve.
#' @param script JS to execute. If a JS Promise is returned, `$executeScript()` will wait for the promise to resolve before returning.
#' @param ... Additional arguments to script.
#' @return Result of the script.
#' @include shiny-driver.R
ShinyDriver2$set("public", "executeScript", function(script, ...) {
  # TODO-barret; incorporate `wait_` parameters to not wait for the _tick_ to finish
  "!DEBUG ShinyDriver2$executeScript"
  chromote_execute_script(
    self$get_chromote_session(),
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
# TODO-barret; Should this be a `timeout` parameter? (Not `timeout_`) Or should all `timeout` parameters be renamed to `timeout_`?
ShinyDriver2$set("public", "execute_script_callback", function(script, ..., timeout_ = 15 * 1000) {
  # TODO-barret; incorporate `wait_` parameters to not wait for the _tick_ to finish
  "!DEBUG ShinyDriver2$execute_script_callback"
  chromote_execute_script_callback(
    self$get_chromote_session(),
    script,
    arguments = rlang::list2(...),
    timeout = timeout_
  )
  invisible(self)
})
