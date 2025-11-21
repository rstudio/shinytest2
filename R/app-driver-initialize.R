app_initialize_ <- function(
  self,
  private,
  app_dir = testthat::test_path("../../"),
  ...,
  load_timeout = missing_arg(),
  timeout = missing_arg(),
  wait = TRUE,
  expect_values_screenshot_args = TRUE,
  screenshot_args = missing_arg(),
  check_names = TRUE,
  name = NULL,
  variant = missing_arg(),
  view = missing_arg(),
  height = NULL,
  width = NULL,
  seed = NULL,
  clean_logs = TRUE,
  shiny_args = list(),
  render_args = NULL,
  options = list()
) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  # This will kill any existing Shiny processes launched by shinytest,
  # in case they're using some of the same resources.
  # https://github.com/rstudio/shinytest2/issues/19#issuecomment-954095837
  gc()

  # Convert `rlang::missing_arg()` to `missing_value()` to allow for R6 printing
  private$default_expect_values_screenshot_args <- maybe_missing_value(
    expect_values_screenshot_args,
    missing_value()
  ) # nolint
  private$default_screenshot_args <- maybe_missing_value(
    screenshot_args,
    missing_value()
  )
  app_set_variant(self, private, variant)

  private$counter <- Count$new()
  private$shiny_url <- Url$new()

  app_init_timeouts(
    self,
    private,
    load_timeout = load_timeout,
    timeout = timeout
  )

  private$save_dir <- st2_temp_file()
  # Clear out any prior files
  if (fs::dir_exists(private$save_dir)) {
    unlink(private$save_dir, recursive = TRUE)
  }
  dir.create(private$save_dir, recursive = TRUE)

  # NULL values are ok! (default)
  private$name <- name
  private$clean_logs <- isTRUE(clean_logs)

  self$log_message("Start AppDriver initialization")

  private$state <- "initialize"

  if (shiny::is.shiny.appobj(app_dir)) {
    app_dir <- app_save(app_dir)
  }

  if (
    length(app_dir) == 1 &&
      is.character(app_dir) &&
      grepl("^http(s?)://", app_dir)
  ) {
    private$shiny_url$set(app_dir)
    app_set_dir(self, private, ".")
  } else {
    "!DEBUG starting shiny app from path"
    self$log_message("Starting Shiny app")
    # Function or path
    app_set_dir(self, private, app_dir)

    app_start_shiny(
      self,
      private,
      seed = seed,
      load_timeout = private$load_timeout,
      shiny_args = shiny_args,
      render_args = render_args,
      options = options
    )
  }

  # Read js content before init'ing chromote to reduce time between
  js_file <- system.file(
    "internal",
    "js",
    "shiny-tracer.js",
    package = "shinytest2"
  )
  js_content <- read_utf8(js_file)

  "!DEBUG create new phantomjs session"
  self$log_message("Creating new ChromoteSession")
  private$chromote_session <- chromote::default_chromote_object()$new_session()

  if (isTRUE(view)) {
    self$view()
  }

  app_init_browser_log(self, private, options = options)

  "!DEBUG navigate to Shiny app"
  if (!is.null(height) || !is.null(width)) {
    ckm8_assert_single_number(height, lower = 1)
    ckm8_assert_single_number(width, lower = 1)
    self$log_message(paste0("Setting window size: ", height, "x", width))
    # Do not wait for shiny values... Have not navigated to Shiny app yet
    self$set_window_size(width = width, height = height, wait = FALSE)
  }
  self$log_message("Navigating to Shiny app")
  # private$web$go(private$shiny_url$get())
  self$get_chromote_session()$Page$navigate(private$shiny_url$get())

  # TODO-future; This feels like it is being loaded too late. There is no guarantee that the script will load in time.
  # This would be nice if it was an HTML dependency to work with page reloading.
  "!DEBUG inject shiny-tracer.js to load before all other scripts"
  self$log_message("Injecting shiny-tracer.js")
  utils::capture.output({
    chromote_eval(
      self$get_chromote_session(),
      js_content
    )
  })

  "!DEBUG waiting for Shiny to become stable"
  self$log_message("Waiting for Shiny to become ready")

  withCallingHandlers(
    # abort() on error
    {
      # nolint
      self$wait_for_js(
        "window.shinytest2 && window.shinytest2.ready === true",
        timeout = private$load_timeout
      )
      if (isTRUE(wait)) {
        # Use value less than the common 250ms/500ms timeout of watching a file for changes
        self$wait_for_idle(duration = 200, timeout = private$load_timeout)
      }
    },
    error = function(e) {
      app_abort(
        self,
        private,
        paste0(
          "Shiny app did not become stable in ",
          private$load_timeout,
          "ms.\n",
          "Message: ",
          conditionMessage(e)
        ),
        parent = e
      )
    }
  )

  "!DEBUG shiny started"
  self$log_message("Shiny app started")
  private$state <- "running"

  private$shiny_worker_id <- chromote_eval(
    self$get_chromote_session(),
    "Shiny.shinyapp.config.workerId"
  )$result$value
  if (identical(private$shiny_worker_id, "")) {
    private$shiny_worker_id <- NA_character_
  }

  private$shiny_test_url <- chromote_eval(
    self$get_chromote_session(),
    "Shiny.shinyapp.getTestSnapshotBaseUrl ? Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl:true}) : null"
  )$result$value

  if (check_names) {
    app_check_unique_names(self, private)
  }

  invisible(self)
}


app_initialize <- function(self, private, ..., view = missing_arg()) {
  ckm8_assert_app_driver(self, private)

  # Always skip on CRAN
  # https://rstudio.github.io/chromote/articles/example-cran-tests.html
  #
  # > Note that the manual says that external commands (here 'chrome') must
  # > be used conditionally, so changes should not have resulted in a check
  # > failure.
  # - Ripley
  #
  # However, we can not use it conditionally.
  # So the only course of action is to skip the test.

  # If sys envvar `SHINYTEST2_APP_DRIVER_TEST_ON_CRAN` is "true" or "1",
  # then do not skip testing on CRAN
  env_test_on_cran <- Sys.getenv("SHINYTEST2_APP_DRIVER_TEST_ON_CRAN", "false")
  should_skip_on_cran <- !tolower(env_test_on_cran) %in% c("true", "1")
  if (should_skip_on_cran) {
    # 99% Common case: Skip test on CRAN where chromote is used
    testthat::skip_on_cran()
  }

  if (testthat::is_testing()) {
    # Make sure chromote can be started. If not, skip test
    try_chromote <- function(silent = FALSE) {
      try(silent = silent, {
        # Should throw an error if Chrome is not found
        chromote::default_chromote_object()$new_session()
      })
    }
    if (on_ci() && is_windows()) {
      # Windows GHA needs a kick start for `{chromote}` to connect
      # https://github.com/rstudio/shinytest2/issues/209
      # Try starting it before checking for it again:
      # https://github.com/rstudio/shinytest2/issues/209#issuecomment-1121465705

      # Do not care about result; Asking again should be fast
      try_chromote(silent = TRUE)
    }

    # Display error if chromote is not found
    chromote_can_be_started <- try_chromote(silent = FALSE)
    if (inherits(chromote_can_be_started, "try-error")) {
      # Skip test
      testthat::skip(
        "`shinytest2::AppDriver` can not be initialized as {chromote} can not be started"
      )
    }
  }

  withCallingHandlers(
    # abort() on error
    app_initialize_(self, private, ..., view = view),
    error = function(e) {
      tryCatch(
        self$log_message(paste0(
          "Error while initializing AppDriver:\n",
          conditionMessage(e)
        )),
        error = function(ee) {
          app_inform(
            self,
            private,
            paste0("Could not log error message. Error: ", conditionMessage(ee))
          )
        }
      )

      # Open chromote session if it is not already open and `view != FALSE`
      # `view` defaults to `rlang::missing_arg()`
      tryCatch(
        {
          view_val <- rlang::maybe_missing(view, NULL)
          if (
            rlang::is_interactive() &&
              # If no chromote session object exists, then we can't view it
              inherits(self$get_chromote_session(), "ChromoteSession") &&
              # If view_val == TRUE, ChromoteSession was opened earlier when possible. Do not open again.
              !isTRUE(view_val) &&
              # If view_val == FALSE, user asked to not open chromote session. Do not open
              !is_false(view_val)
          ) {
            app_inform(
              self,
              private,
              "`$view()`ing chromote session for debugging purposes"
            )
            self$log_message("Viewing chromote session for debugging purposes")
            self$view()
          }
        },
        error = function(ee) {
          app_inform(
            self,
            private,
            c(
              "!" = paste0(
                "Could not open chromote session. Error: ",
                conditionMessage(ee)
              )
            )
          )
        }
      )

      logs <- tryCatch(
        format(self$get_logs()),
        error = function(e) "(Error retrieving logs)"
      )

      app_abort(
        self,
        private,
        c(
          conditionMessage(e),
          "\n",
          i = cli::col_silver(
            "You can inspect the failed AppDriver object via `rlang::last_error()$app`"
          ),
          i = paste0("AppDriver logs:\n", logs),
          "\n"
        ),
        parent = e
      )
    }
  )
}
