app_upload_file <- function(
  self, private,
  ...,
  wait_ = TRUE,
  timeout_ = 3000
) {
  ckm8_assert_app_driver(self, private)

  inputs <- list2(...)
  if (length(inputs) != 1 || !rlang::is_named(inputs)) {
    app_abort(self, private,
      "Can only upload file to exactly one input, and input must be named"
    )
  }

  # Wait for two messages by calling `.start(timeout, 2)`. This is because
  # uploading a file will result in two messages before the file is successfully
  # uploaded.
  self$run_js(paste0(
    "shinytest2.outputValuesWaiter.start(", toJSON_atomic(timeout_), ", 2);"
  ))

  self$log_message(paste0("Uploading file", input = inputs[[1]]))

  node_id <- app_find_node_id(self, private, input = names(inputs)[1])
  filename <- inputs[[1]]
  self$get_chromote_session()$DOM$setFileInputFiles(
    files = list(fs::path_abs(filename)),
    nodeId = node_id
  )

  self$get_js(paste0(
    "
    new Promise((resolve, reject) => {
      shinytest2.outputValuesWaiter.finish(", toJSON_atomic(wait_), ", resolve);
    });
    "
  ))

  # Need to wait for the progress bar's CSS transition to complete. The
  # transition is 0.6s, so this will ensure that it's done.
  Sys.sleep(0.6)

  self$log_message("Finished uploading file")

  invisible()
}
