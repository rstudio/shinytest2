assert_chromote <- function(chromote) {
  checkmate::assert_class(chromote, c("ChromoteSession", "R6"))
}

# node_id <- function(node) {
#   if (is.null(node)) return(NULL)
#   checkmate::assert_list(node, any.missing = FALSE)

#   node$nodeId
# }


chromote_eval <- function(chromote, js, ..., wait_ = TRUE) {
  assert_chromote(chromote)
  checkmate::assert_character(js, any.missing = FALSE, len = 1)

  cat("\n", js, "\n")

  result <-
    withCallingHandlers(
      {
        # https://chromedevtools.github.io/devtools-protocol/tot/Runtime/#method-evaluate
        chromote$
          Runtime$
          evaluate(js, ..., wait_ = wait_)
      },
      error = function(e) {
        # Return something similar to a timeout object
        # Ex: `chromote_wait_for_condition(b, "false", timeout_ms = 100)`
        list(results = list(
          type = "object",
          subtype = "error",
          className = "Error",
          description = paste0("Error found while trying to evaluate script: ", as.character(e))
        ))
      }
    )

  post_result <- function(ret_val) {
    if (identical(ret_val$result$subtype, "error")) {
      message(
        "chromote ", ret_val$exceptionDetails$lineNumber, ":", ret_val$exceptionDetails$columnNumber,
        " : ",
        ret_val$result$description
      )
      # str(ret_val)
    }
    ret_val
  }
  if (wait_) {
    post_result(result)
  } else {
    result$then(post_result)
  }

}





#' Evaluate a script on the window context
#'
#' @param arguments V
#' @importFrom rlang %||%
chromote_eval_script <- function(chromote, js_script, awaitPromise = TRUE, arguments = NULL, timeout = 10 * 1000, ...) {
  assert_chromote(chromote)
  checkmate::assert_character(js_script, any.missing = FALSE, len = 1)
  utils::str(rlang::names2(arguments %||% ""))
  checkmate::assert_true(isTRUE(all(rlang::names2(arguments %||% "") == "")))

  args_serialized <- toJSON(arguments)

  js_script <- paste0("(function() { ", js_script, " }).apply(null, ", args_serialized, ");")
  print(js_script)

  chromote_eval(
    chromote,
    js_script,
    awaitPromise = awaitPromise,
    timeout = timeout,
    ...
  )
}
#' Evaluate on the window context, blocking until the wrapping promise is resolved
chromote_eval_script_callback <- function(chromote, js_script, awaitPromise = TRUE, arguments = NULL, timeout = 10 * 1000, ...) {
  stop("TODO-barret")
  assert_chromote(chromote)
  checkmate::assert_character(js_script, any.missing = FALSE, len = 1)
  checkmate::assert_true(isTRUE(rlang::names2(arguments %||% "") == ""))

  args_serialized <- toJSON(arguments)

  js_script <- paste0("(function() { ", js_script, " }).apply(null, ", args_serialized, ");")
  print(js_script)

  chromote_eval(
    chromote,
    js_script,
    awaitPromise = awaitPromise,
    timeout = timeout,
    ...
  )
}



#' Have chromote wait for a js condition to be met
#'
#' @return `TRUE` if expression evaluates to `true` without error, before
#'   timeout. Otherwise returns `NA`.
#' @noRd
chromote_wait_for_condition <- function(chromote, condition_js, timeout_ms = 30 * 1000, delay_ms = 100) {
  assert_chromote(chromote)
  checkmate::assert_character(condition_js, any.missing = FALSE, len = 1)
  checkmate::assert_number(timeout_ms, lower = 0)
  checkmate::assert_number(delay_ms, lower = 0)

  js <- paste0(
    "
    new Promise(function (resolve, reject) {
      var start = new Date();
      function condition() {
        return ", condition_js, ";
      };
      // Use `chromote_wait_for_condition` as the error message matches the R method
      function chromote_wait_for_condition() {
        var diffTime = new Date() - (+start + ", timeout_ms, ");
        if (diffTime > 0) {
          return reject(new Error('Timeout waiting for condition'));
        }
        if (condition()) {
          return resolve();
        }
        setTimeout(chromote_wait_for_condition, ", delay_ms, ");
      }
      chromote_wait_for_condition();
    });
    "
  )
  ret <- chromote_eval(
    chromote,
    js,
    # Wait for promise to finish
    awaitPromise = TRUE,
    # use Inf timeout to let promise fail on timeout instead
    timeout_ = Inf
  )
  if (identical(ret$result$subtype, "error")) {
    return(NA)
  }

  TRUE
}




chromote_set_device_metrics <- function(chromote, ..., width = NULL, height = NULL, deviceScaleFactor = NULL, mobile = NULL) {
  assert_chromote(chromote)
  ellipsis::check_dots_empty()

  chromote$Emulation$setDeviceMetricsOverride(
    width = width,
    height = height,
    deviceScaleFactor = deviceScaleFactor,
    mobile = mobile
  )
}


chromote_set_window_size <- function(chromote, width, height) {
  assert_chromote(chromote)
  chromote_set_device_metrics(chromote, width = width, height = height)
}


chromote_root_node_id <- function(chromote) {
  assert_chromote(chromote)

  # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-getDocument
  chromote$DOM$getDocument()$root$nodeId
}


chromote_find_element <- function(chromote, css, rootId = chromote_root_node_id(chromote)) {
  assert_chromote(chromote)
  checkmate::assert_character(css, any.missing = FALSE, len = 1)

  # Returns a list of nodeId values
  # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-querySelector
  node_id <- chromote$DOM$querySelector(rootId, css)$nodeId

  # Do not return a missing nodeId
  if (node_id == 0L) return(NULL)
  node_id
}


chromote_find_elements <- function(chromote, css, rootId = chromote_root_node_id(chromote)) {
  assert_chromote(chromote)
  checkmate::assert_character(css, any.missing = FALSE, len = 1)

  # If `unlist()`ing an empty `list()`, `NULL` is returned
  unlist(
    # Returns a list of nodeId values
    # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-querySelectorAll
    chromote$DOM$querySelectorAll(rootId, css)$nodeIds
  )
}







chromote_eval_script_on_document <- function(chromote, js_script, awaitPromise = TRUE, arguments = NULL, timeout = Inf, ...) {
  assert_chromote(chromote)
  checkmate::assert_character(js_script, any.missing = FALSE, len = 1)

  chromote_call_js_on_node(
    chromote,
    # To get access to arugments, we need to call js on a node.
    # Let's use the root node
    chromote_root_node_id(chromote),
    fn_js = paste0("function() { ", js_script, " }"),
    # Wait for promise to finish
    awaitPromise = awaitPromise,
    arguments = arguments,
    # use Inf timeout to let promise fail on timeout instead
    timeout_ = timeout,
    ...
  )
}

chromote_node_id_to_object_id <- function(chromote, node_id) {
  assert_chromote(chromote)
  checkmate::assert_integer(node_id, lower = 0)

  # https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-resolveNode
  chromote$DOM$resolveNode(node_id)$object$objectId
}

chromote_call_js_on_object <- function(chromote, object_id, fn_js, ...) {
  assert_chromote(chromote)
  checkmate::assert_character(object_id, any.missing = FALSE, len = 1)
  checkmate::assert_character(fn_js, any.missing = FALSE, len = 1)

  # https://chromedevtools.github.io/devtools-protocol/tot/Runtime/#method-callFunctionOn
  chromote$Runtime$callFunctionOn(fn_js, objectId = object_id, ...)
}

chromote_call_js_on_node <- function(chromote, node_id, fn_js, ...) {
  object_id <- chromote_node_id_to_object_id(chromote, node_id)
  chromote_call_js_on_object(chromote, object_id, fn_js, ...)
}
