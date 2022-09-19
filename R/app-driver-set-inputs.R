app_set_inputs <- function(
  self, private,
  ...,
  wait_ = TRUE,
  timeout_ = missing_arg(),
  allow_no_input_binding_ = FALSE,
  priority_ = c("input", "event")
) {
  ckm8_assert_app_driver(self, private)
  timeout_ <- app_get_timeout(self, private, timeout = timeout_)

  priority_ <- match.arg(priority_)

  inputs <- list2(...)
  input_values <- lapply(inputs, function(value) {
    list(
      value = value,
      allowNoInputBinding = allow_no_input_binding_,
      priority = priority_
    )
  })

  self$log_message(
    paste0("Setting inputs: ", paste0("'", names(input_values), "'", collapse = "', '"))
  )

  app_queue_inputs(self, private, input_values)
  res <- app_flush_inputs(self, private, wait = wait_, timeout = timeout_)
  if (is.character(res)) {
    msg <- paste0("Error received while setting inputs: ", res)
    self$log_message(msg)
    app_inform_where(self, private, msg)
    return(invisible(self))
  }

  if (isTRUE(res$timedOut)) {
    # Get the text from one call back, like "app$set_inputs(a=1, b=2)"
    calls <- sys.calls()
    call_text <- deparse(calls[[length(calls) - 1]])

    app_inform_where(self, private, paste0(
      "set_inputs(", call_text, "): ",
      "Server did not update any output values within ",
      format(timeout_ / 1000, digits = 2), " seconds. ",
      "If this is expected, use `wait_ = FALSE` or increase the value of `timeout_`."
    ))
  }

  self$log_message(paste0("Finished setting inputs. Timedout: ", isTRUE(res$timedOut)))

  invisible(self)
}

app_queue_inputs <- function(self, private, inputs) {
  ckm8_assert_app_driver(self, private)
  checkmate::assert_true(rlang::is_named(inputs))

  self$run_js(
    paste0("shinytest2.inputQueue.add(", toJSON(inputs), ");")
  )
}
app_flush_inputs <- function(self, private, wait = TRUE, timeout = 1000) {
  ckm8_assert_app_driver(self, private)
  wait <- isTRUE(wait)
  checkmate::assert_number(timeout, lower = 0, finite = TRUE, null.ok = FALSE)

  self$get_js(
    paste0("
    new Promise((resolve, reject) => {
      shinytest2.outputValuesWaiter.start(", toJSON_atomic(timeout), ");
      shinytest2.inputQueue.flush();
      shinytest2.outputValuesWaiter.finish(", toJSON_atomic(wait), ", resolve);
    });
    "),
    timeout = 2 * timeout # Don't let chromote timeout before we do
  )
}
