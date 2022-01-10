assert_chromote_session <- function(chromote_session) {
  checkmate::assert_class(chromote_session, c("ChromoteSession", "R6"))
}

# node_id <- function(node) {
#   if (is.null(node)) return(NULL)
#   checkmate::assert_list(node, any.missing = FALSE)

#   node$nodeId
# }

# TODO-barret-actual; implement wait_ logic using all promises and `chromote_session$waitFor(p)`
chromote_eval <- function(
  chromote_session,
  js,
  ...,
  timeout = 10 * 1000, # milliseconds for chrome devtools protocol
  # TODO-future; Remove `timeout_` parameter; https://github.com/rstudio/chromote/pull/69
  timeout_ = timeout * 2 / 1000, # seconds for chromote to timeout; Default to double wall time
  awaitPromise = TRUE, # nolint
  returnByValue = TRUE, # nolint
  wait_ = TRUE
) {
  assert_chromote_session(chromote_session)
  checkmate::assert_character(js, any.missing = FALSE, len = 1)
  checkmate::assert_true(returnByValue) # Many internal functions depend on this logic
  checkmate::assert_true(awaitPromise) # Many internal functions depend on this logic
  checkmate::assert_true(wait_) # All internal functions depend on this logic

  # cat("\n", js, "\n")

  result <-
    withCallingHandlers(
      {
        # https://chromedevtools.github.io/devtools-protocol/tot/Runtime/#method-evaluate
        chromote_session$
          Runtime$
          evaluate(
            js,
            ...,
            timeout = timeout,
            timeout_ = timeout_,
            awaitPromise = TRUE,
            returnByValue = TRUE,
            wait_ = TRUE
          )
      },
      error = function(e) {
        # Return something similar to a timeout object
        # Ex: `chromote_wait_for_condition(b, "false", timeout = 100)`
        list(results = list(
          type = "object",
          subtype = "error",
          className = "Error",
          description = paste0("Error found while trying to evaluate script: ", as.character(e))
        ))
      }
    )

  # if (identical(result$result$subtype, "error")) {
  #   message(
  #     "chromote_session ", result$exceptionDetails$lineNumber, ":", result$exceptionDetails$columnNumber,
  #     " : ",
  #     result$result$description
  #   )
  #   # str(result)
  # }
  result

}



#' Execute a JavaScript script
#'
#' `chromote_execute_script()` will block the testing R session
#' until the script has completed the JS execution tick within the headless browser.
#'
#'
#' @param chromote_session A ChromoteSession object
#' @param script A string containing the script to be evaluated
#' @param ... Should be empty
#' @param eval_args Arguments passed to `chromote_session$Runtime$evaluate`
#' @param arguments An unnamed list of arguments to be passed into the `script`
#' @param timeout The maximum time (milliseconds) `chromote_session` will wait for the `script` to resolved
#' @importFrom rlang %||%
#' @describeIn chromote_execute_script Executes the supplied JavaScript script (`script`) within a function. The function has the `window` context and access to `arguments` supplied.
#' @noRd
chromote_execute_script <- function(
  chromote_session,
  script,
  ...,
  arguments = list(),
  eval_args = list(),
  timeout = 10 * 1000
) {
  assert_chromote_session(chromote_session)
  ellipsis::check_dots_empty()

  checkmate::assert_true(!rlang::is_named(arguments))
  checkmate::assert_character(script, any.missing = FALSE, len = 1)

  args_serialized <- toJSON(arguments)
  script <- paste0("(function() { ", script, " }).apply(null, ", args_serialized, ");")
  rlang::exec(
    .fn = chromote_eval,
    chromote_session,
    script,
    timeout = timeout,
    !!!eval_args
  )
}

# #' @describeIn chromote_execute_script Executes the supplied JavaScript script (`script`) within a function. The function has the `window` context and access to `arguments` supplied. An extra argument (`resolve(val)`) is added to the `arguments` list. If `wait_ = TRUE`, then `chromote_execute_script_callback()` will block the main R session until `resolve()` has been called.
# #' @noRd
# chromote_execute_script_callback <- function( # nolint
#   chromote_session,
#   script,
#   ...,
#   arguments = list2(),
#   timeout = 15 * 1000,
#   wait_ = TRUE
# ) {
#   stop("this should not be called!")
#   if ("awaitPromise" %in% rlang::names2(list2(...))) {
#     stop("`awaitPromise` can not be supplied to chromote_execute_script_callback()")
#   }
#   checkmate::assert_character(script, any.missing = FALSE, len = 1)

#   # 1. Pass in user args (via `chromote_execute_script()`)
#   # 2. Append the resolve function to args
#   #   * We MUST wrap user code in a `function()` as arrow functions `=>` do not support `arguments`
#   #   * By having the wrapping `Promise` contain an arrow function, then `arguments` does not get a new binding
#   #     https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions#no_binding_of_arguments
#   # 3. Call fn w/ user args and resolve function.
#   #   * The fn contents should be the user script and have `window` as the context
#   #   * This allows for the user to be none the wiser about where `callback` comes from.
#   #   * `callback` is a function that only takes a `value` argument.
#   #   * There is no ability to `reject()`.

#   script <- paste0(
# "return new Promise((resolve, reject) => {\n",
# "  (function() {\n",
#     script, "\n",
# # Call fn w/ user arguments and resolve function using the `window` context
# "  }).apply(null, [...arguments, resolve, reject]);
# });"
#   )
#   chromote_execute_script(chromote_session, script, ..., awaitPromise = wait_, arguments = arguments, timeout = timeout, wait_ = wait_)
# }



#' Have chromote wait for a js condition to be met
#'
#' @param condition_js A piece of JavaScript code that should eventually evaluate to a [`true`thy value](https://developer.mozilla.org/en-US/docs/Glossary/Truthy).
#' @param interval How long (milliseconds) Chrome should wait between checking `condition_js`
#' @return `invisible(chromote_session)` if expression evaluates to `TRUE` without error, before
#'   timeout. Otherwise an error is thrown.
#' @noRd
#' @importFrom rlang list2
chromote_wait_for_condition <- function(
  chromote_session,
  condition_js,
  ...,
  arguments = list2(),
  eval_args = list2(),
  timeout = 15 * 1000,
  interval = 100
) {
  ellipsis::check_dots_empty()
  checkmate::assert_character(condition_js, any.missing = FALSE, len = 1)
  checkmate::assert_number(timeout, lower = 0)
  checkmate::assert_number(interval, lower = 0)

  # Must use manual calulation of timeout, as `chromote_session` does not have a
  # way to cancel the `setTimeout` that has already been submitted. (Which will never stop resubmitting)
  script <- paste0(
# `callback` provided by chromote_execute_script_callback()
"return new Promise((resolve, reject) => {
  let start = Date.now();
  const condition = () => {
    return ", condition_js, ";
  };\n",
  # Use `chromote_wait_for_condition` as the error message matches the R method
  "chromote_wait_for_condition = () => {
    let diffTime = new Date() - (+start + ", timeout, ");
    if (diffTime > 0) {
      return reject('Timeout waiting for JS condition to be `true`');
    }
    if (condition()) {
      return resolve();
    }
    setTimeout(chromote_wait_for_condition, ", interval, ");
  }
  chromote_wait_for_condition();
  });"
  )
  ret <- chromote_execute_script(
    chromote_session,
    script,
    eval_args = eval_args,
    ## Supply a large "wall time" to chrome devtools protocol. The manual logic should be hit first
    timeout = timeout * 2
  )

  if (identical(ret$result$subtype, "error") || length(ret$exceptionDetails) > 0) {
    abort("Timeout waiting for JavaScript condition to be `true`")
  }

  invisible(chromote_session)
}




chromote_set_device_metrics <- function(chromote_session, ..., width = NULL, height = NULL, device_scale_factor = 1, mobile = FALSE) {
  assert_chromote_session(chromote_session)
  ellipsis::check_dots_empty()

  chromote_session$Emulation$setDeviceMetricsOverride(
    width = width,
    height = height,
    deviceScaleFactor = device_scale_factor,
    mobile = mobile
  )
}


chromote_set_window_size <- function(chromote_session, width, height) {
  assert_chromote_session(chromote_session)
  chromote_set_device_metrics(chromote_session, width = width, height = height)
}


chromote_root_node_id <- function(chromote_session) {
  assert_chromote_session(chromote_session)

  # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-getDocument
  chromote_session$DOM$getDocument()$root$nodeId
}


chromote_find_element <- function(chromote_session, css, root_id = chromote_root_node_id(chromote_session)) {
  assert_chromote_session(chromote_session)
  checkmate::assert_character(css, any.missing = FALSE, len = 1)

  # Returns a list of nodeId values
  # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-querySelector
  node_id <- chromote_session$DOM$querySelector(root_id, css)$nodeId

  # Do not return a missing nodeId
  if (node_id == 0L) return(NULL)
  node_id
}


chromote_find_elements <- function(chromote_session, css, root_id = chromote_root_node_id(chromote_session)) {
  assert_chromote_session(chromote_session)
  checkmate::assert_character(css, any.missing = FALSE, len = 1)

  # If `unlist()`ing an empty `list()`, `NULL` is returned
  unlist(
    # Returns a list of nodeId values
    # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-querySelectorAll
    chromote_session$DOM$querySelectorAll(root_id, css)$nodeIds
  )
}






# nolint start
chromote_execute_script_on_document <- function(chromote_session, script, awaitPromise = TRUE, arguments = list(), timeout_ = Inf, ...) {
  assert_chromote_session(chromote_session)
  checkmate::assert_character(script, any.missing = FALSE, len = 1)

  chromote_call_js_on_node(
    chromote_session,
    # To get access to arugments, we need to call js on a node.
    # Let's use the root node
    chromote_root_node_id(chromote_session),
    fn_js = paste0("function() { ", script, " }"),
    # Wait for promise to finish
    awaitPromise = awaitPromise,
    arguments = arguments,
    # use Inf timeout to let promise fail on timeout instead
    timeout_ = timeout_,
    ...
  )
}

chromote_node_id_to_object_id <- function(chromote_session, node_id) {
  assert_chromote_session(chromote_session)
  checkmate::assert_integer(node_id, lower = 0)

  # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-resolveNode
  chromote_session$DOM$resolveNode(node_id)$object$objectId
}

chromote_call_js_on_object <- function(chromote_session, object_id, fn_js, ..., returnByValue = TRUE) {
  assert_chromote_session(chromote_session)
  checkmate::assert_character(object_id, any.missing = FALSE, len = 1)
  checkmate::assert_character(fn_js, any.missing = FALSE, len = 1)

  # https://chromedevtools.github.io/devtools-protocol/tot/Runtime/#method-callFunctionOn
  chromote_session$Runtime$callFunctionOn(fn_js, objectId = object_id, ..., returnByValue = returnByValue)
}

chromote_call_js_on_node <- function(chromote_session, node_id, fn_js, ...) {
  object_id <- chromote_node_id_to_object_id(chromote_session, node_id)
  chromote_call_js_on_object(chromote_session, object_id, fn_js, ...)
}
# nolint end
