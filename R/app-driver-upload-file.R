app_upload_file <- function(
  self, private,
  ...,
  wait_ = TRUE,
  values_ = TRUE,
  timeout_ = 3000
) {
  ckm8_assert_app_driver(self, private)

  if (values_ && !wait_) {
    abort(c(
      "values_=TRUE and wait_=FALSE are not compatible.",
      "Can't return all values without waiting for update."
    ))
  }

  inputs <- list2(...)
  if (length(inputs) != 1 || !rlang::is_named(inputs)) {
    abort("Can only upload file to exactly one input, and input must be named")
  }

  # Wait for two messages by calling `.start(timeout, 2)`. This is because
  # uploading a file will result in two messages before the file is successfully
  # uploaded.
  self$execute_script(
    "var timeout = arguments[0];
    shinytest2.outputValuesWaiter.start(timeout, 2);",
    arguments = list(timeout_)
  )

  self$log_event("Uploading file", input = inputs[[1]])

  node_id <- app_find_node_id(self, private, id = names(inputs)[1], iotype = "input")
  filename <- inputs[[1]]
  self$get_chromote_session()$DOM$setFileInputFiles(
    files = list(fs::path_abs(filename)),
    nodeId = node_id
  )

  self$execute_script_callback(
    "var wait = arguments[0];
    var resolve = arguments[1];
    shinytest2.outputValuesWaiter.finish(wait, resolve);",
    arguments = list(wait_)
  )

  # Need to wait for the progress bar's CSS transition to complete. The
  # transition is 0.6s, so this will ensure that it's done.
  Sys.sleep(0.6)

  self$log_event("Finished uploading file")

  if (values_)
    invisible(self$get_values())
  else
    invisible()
}
