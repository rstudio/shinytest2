#' @description
#' Sets input values.
#' @param ... Name-value pairs, `name1 = value1, name2 = value2` etc.
#'   Enput with name `name1` will be assigned value `value1`.
#' @param allowInputNoBinding_ When setting the value of an input, allow
#'   it to set the value of an input even if that input does not have
#'   an input binding.
#' @param priority_ Sets the event priority. For expert use only: see
#'   <https://shiny.rstudio.com/articles/communicating-with-js.html#values-vs-events> for details.
#' @param values_ If `TRUE`, will return final updated values of inputs.
#' @return Returns updated values, invisibly.
#' @include shiny-driver.R
ShinyDriver2$set("public", "setInputs", function(
  ...,
  wait_ = TRUE,
  values_ = TRUE,
  timeout_ = 3000,
  allowInputNoBinding_ = FALSE,
  priority_ = c("input", "event")
) {
  if (values_ && !wait_) {
    abort(c(
      "values_=TRUE and wait_=FALSE are not compatible.",
      "Can't return all values without waiting for update."
    ))
  }

  priority_ <- match.arg(priority_)

  inputs <- list2(...)
  input_values <- lapply(inputs, function(value) {
    list(
      value = value,
      allowInputNoBinding = allowInputNoBinding_,
      priority = priority_
    )
  })

  self$logEvent("Setting inputs",
    input = paste(names(input_values), collapse = ",")
  )

  private$queueInputs(input_values)
  res <- private$flushInputs(wait_, timeout_)

  if (isTRUE(res$timedOut)) {
    # Get the text from one call back, like "app$setInputs(a=1, b=2)"
    calls <- sys.calls()
    call_text <- deparse(calls[[length(calls) - 1]])

    inform_where(paste0(
      "setInputs(", call_text, "): ",
      "Server did not update any output values within ",
      format(timeout_ / 1000, digits = 2), " seconds. ",
      "If this is expected, use `wait_=FALSE, values_=FALSE`, or increase the value of timeout_."
    ))
  }

  self$logEvent("Finished setting inputs", timedout = res$timedOut)

  values <- NULL
  if (values_) {
    values <- self$getAllValues()
  }


  invisible(values)
})


#' @include shiny-driver.R
ShinyDriver2$set("private", "queueInputs", function(inputs) {
  checkmate::assert_true(rlang::is_named(inputs))

  # todo-barret; make sure it works
  self$executeScript(
    "shinytest2.inputQueue.add(arguments[0]);",
    inputs
  )
})

#' @include shiny-driver.R
ShinyDriver2$set("private", "flushInputs", function(wait = TRUE, timeout = 1000) {
  # TODO-barret; make sure it works
  # TODO-barret; where does `callback` come from?
  # TODO-barret; use self$executeScriptCallback
  self$executeScript(
    "
    var wait = arguments[0];
    var timeout = arguments[1];
    new Promise(function(resolve, reject) {
      var callback = resolve;
      shinytest2.outputValuesWaiter.start(timeout);
      shinytest2.inputQueue.flush();
      shinytest2.outputValuesWaiter.finish(wait, callback);
    })
    ",
    wait,
    timeout
  )
  # # self$blockingUntilCallbackWithExecuteScriptAsync()
  # self$executeScriptAsync(
  #   "var wait = arguments[0];
  #   var timeout = arguments[1];
  #   var callback = arguments[2];
  #   shinytest2.outputValuesWaiter.start(timeout);
  #   shinytest2.inputQueue.flush();
  #   shinytest2.outputValuesWaiter.finish(wait, callback);",
  #   wait,
  #   timeout
  # )
})


#' @description
#' Uploads a file to a file input.
#' @param ... Name-path pairs, e.g. `name1 = path1`. The file located at
#' `path1` will be uploaded to file input with name `name1`.
#' @param values_ If `TRUE`, will return final updated values of download
#'   control.
ShinyDriver2$set("public", "uploadFile", function(..., wait_ = TRUE, values_ = TRUE,
                          timeout_ = 3000) {
  # TODO-barret; Implement this; https://github.com/rstudio/shinytest2/issues/20
  stop("TODO-barret; ShinyDriver2$uploadFile()")
  if (values_ && !wait_) {
    abort(c(
      "values_=TRUE and wait_=FALSE are not compatible.",
      "Can't return all values without waiting for update."
    ))
  }

  inputs <- list2(...)
  if (length(inputs) != 1 || !is_all_named(inputs)) {
    abort("Can only upload file to exactly one input, and input must be named")
  }

  # Wait for two messages by calling `.start(timeout, 2)`. This is because
  # uploading a file will result in two messages before the file is successfully
  # uploaded.
  private$chromote_obj$executeScript(
    "var timeout = arguments[0];
    shinytest2.outputValuesWaiter.start(timeout, 2);",
    timeout_
  )

  self$logEvent("Uploading file", input = inputs[[1]])

  self$findWidget(names(inputs)[1])$uploadFile(inputs[[1]])

  private$chromote_obj$executeScriptAsync(
    "var wait = arguments[0];
    var callback = arguments[1];
    shinytest2.outputValuesWaiter.finish(wait, callback);",
    wait_
  )

  # Need to wait for the progress bar's CSS transition to complete. The
  # transition is 0.6s, so this will ensure that it's done.
  Sys.sleep(0.6)

  self$logEvent("Finished uploading file")

  if (values_)
    invisible(self$getAllValues())
  else
    invisible()
})
