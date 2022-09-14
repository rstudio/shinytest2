assert_chromote_session <- function(chromote_session) {
  checkmate::assert_class(chromote_session, c("ChromoteSession", "R6"))
}

#' Execute a JavaScript script
#'
#' `chromote_eval()` will block the testing R session
#' until the script has completed the JS execution tick within the headless browser.
#'
#'
#' @param chromote_session A ChromoteSession object
#' @param js A string containing the script to be evaluated
#' @param ... Arguments passed to `chromote_session$Runtime$evaluate`
#' @param timeout The maximum time (milliseconds) `chromote_session` will wait for the `script` to resolved
#' @importFrom rlang %||%
#' @describeIn chromote_eval Executes the supplied JavaScript script (`script`) within a function. The function has the `window` context.
#' @noRd
chromote_eval <- function(
  chromote_session,
  js,
  ...,
  timeout = 10 * 1000, # milliseconds for chrome devtools protocol
  # https://github.com/rstudio/chromote/pull/69
  timeout_ = missing_arg(),
  # nolint start
  awaitPromise = TRUE, # Many internal functions depend on this {chromote} logic;
  returnByValue = TRUE, # Many internal functions depend on this {chromote} logic;
  # nolint end
  allow_no_response = FALSE # Allows for `awaitPromise` and `returnByValue` to be `FALSE`. No function should use this except for: `$run_js()`
) {
  assert_chromote_session(chromote_session)
  checkmate::assert_character(js, any.missing = FALSE, len = 1)
  if (isTRUE(allow_no_response)) {
    checkmate::assert_false(awaitPromise)
    checkmate::assert_false(returnByValue)
  } else {
    checkmate::assert_true(awaitPromise)
    checkmate::assert_true(returnByValue)
  }

  # Wrap in curly braces to scope `let` / `const` variables
  js <- paste0("{\n", js, "\n}")
  # cat("\n", js, "\n")

  result <-
    tryCatch(
      {
        # https://chromedevtools.github.io/devtools-protocol/tot/Runtime/#method-evaluate
        chromote_session$
          Runtime$
          evaluate(
            js,
            ...,
            timeout = timeout,
            timeout_ = timeout_,
            awaitPromise = awaitPromise,
            returnByValue = returnByValue,
            wait_ = TRUE # All internal functions depend on this logic
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

  result
}



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
  timeout = 15 * 1000,
  interval = 100
) {
  ellipsis::check_dots_empty()
  checkmate::assert_character(condition_js, any.missing = FALSE, len = 1)
  checkmate::assert_number(timeout, lower = 0)
  checkmate::assert_number(interval, lower = 0)

  # Escape the user's JS text so that it can be run as is within `eval()`.
  # While `eval(user_txt)` is a security risk, we already allow any user js code to be run via `AppDriver$run_js()`
  # https://github.com/rstudio/shinytest2/issues/236
  escaped_condition_js <- paste0(deparse(condition_js, width.cutoff = 500L), collapse = " ")

  # Must use manual calulation of timeout, as `chromote_session` does not have a
  # way to cancel the `setTimeout` that has already been submitted. (Which will never stop resubmitting)
  script <- paste0(
"new Promise((resolve, reject) => {
  let start = Date.now();
  const condition = () => {
    return eval(", escaped_condition_js, ");
  };\n",
  # Use `chromote_wait_for_condition` as the error message matches the R method
  "chromote_wait_for_condition = () => {
    let diffTime = new Date() - (+start + ", timeout, ");
    if (diffTime > 0) {
      return reject('Timed out waiting for JavaScript script to return `true`');
    }
    try {
      if (condition()) {
        return resolve();
      }
    } catch (e) {
      reject(e);
    }
    setTimeout(chromote_wait_for_condition, ", interval, ");
  }
  chromote_wait_for_condition();
});"
  )
  ret <- chromote_eval(
    chromote_session,
    script,
    ## Supply a large "wall time" to chrome devtools protocol. The manual logic should be hit first
    timeout = timeout * 2
  )

  if (length(ret$exceptionDetails) > 0) {
    # Must match JS txt above!
    if (isTRUE(grepl("Timed out waiting for JavaScript script", ret$exceptionDetails$exception$description, fixed = TRUE))) {
      ## Example `ret`:
      # List of 2
      #  $ result          :List of 2
      #   ..$ type : chr "string"
      #   ..$ value: chr "Timeout waiting for JS condition to be `true`"
      #  $ exceptionDetails:List of 5
      #   ..$ exceptionId : int 2
      #   ..$ text        : chr "Uncaught (in promise)"
      #   ..$ lineNumber  : int 0
      #   ..$ columnNumber: int 0
      #   ..$ exception   :List of 2
      #   .. ..$ type : chr "string"
      #   .. ..$ value: chr "Timeout waiting for JS condition to be `true`"
      rlang::abort(c(
        "Timed out waiting for JavaScript script to return `true`",
        "*" = paste0("Script:\n", condition_js)
      ))
    }

    ## Example `ret`:
    # List of 2
    #  $ result          :List of 5
    #   ..$ type       : chr "object"
    #   ..$ subtype    : chr "error"
    #   ..$ className  : chr "SyntaxError"
    #   ..$ description: chr "SyntaxError: Unexpected token ';'"
    #   ..$ objectId   : chr "7228422962995412097.4.1"
    #  $ exceptionDetails:List of 6
    #   ..$ exceptionId : int 1
    #   ..$ text        : chr "Uncaught"
    #   ..$ lineNumber  : int 3
    #   ..$ columnNumber: int 7
    #   ..$ scriptId    : chr "24"
    #   ..$ exception   :List of 5
    #   .. ..$ type       : chr "object"
    #   .. ..$ subtype    : chr "error"
    #   .. ..$ className  : chr "SyntaxError"
    #   .. ..$ description: chr "SyntaxError: Unexpected token ';'"
    #   .. ..$ objectId   : chr "7228422962995412097.4.2"
    rlang::abort(c(
      "Error found while waiting for JavaScript script to return `true`.",
      "*" = paste0("Script:\n", condition_js),
      "*" = paste0("Exception:\n", obj_to_string(ret$exceptionDetails$exception))
    ))
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

# Method used by `$click(selector=)` and `$upload_file()`
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



chromote_node_id_to_object_id <- function(chromote_session, node_id) {
  assert_chromote_session(chromote_session)
  checkmate::assert_integer(node_id, lower = 0)

  # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-resolveNode
  chromote_session$DOM$resolveNode(node_id)$object$objectId
}


# Methods for `$click()` to be able to call `this.click()`
chromote_call_js_on_object <- function(
  chromote_session,
  object_id,
  fn_js,
  ...,
  # Return the value of the fn
  returnByValue = TRUE # nolint
) {
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
