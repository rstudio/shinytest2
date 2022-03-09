app_initialize_ <- function(
  self, private,
  app_dir = testthat::test_path("../../"),
  ...,
  load_timeout = NULL,
  expect_values_screenshot_args = expect_values_screenshot_args,
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
  ellipsis::check_dots_empty()

  # This will kill any existing Shiny processes launched by shinytest,
  # in case they're using some of the same resources.
  # https://github.com/rstudio/shinytest2/issues/19#issuecomment-954095837
  gc()

  # Convert `rlang::missing_arg()` to `missing_value()` to allow for R6 printing
  private$default_expect_values_screenshot_args <- maybe_missing_value(expect_values_screenshot_args, missing_value()) # nolint
  private$default_screenshot_args <- maybe_missing_value(screenshot_args, missing_value())
  app_set_variant(self, private, variant)

  private$counter <- Count$new()
  private$shiny_url <- Url$new()

  private$save_dir <- temp_file()
  # Clear out any prior files
  if (fs::dir_exists(private$save_dir)) {
    unlink(private$save_dir, recursive = TRUE)
  }
  dir.create(private$save_dir, recursive = TRUE)

  # NULL values are ok! (default)
  private$name <- name
  private$clean_logs <- isTRUE(clean_logs)
  if (is.null(load_timeout)) {
    load_timeout <- if (on_ci()) 20 * 1000 else 10 * 1000
  }

  self$log_message("Start AppDriver initialization")

  private$state <- "initialize"

  if (shiny::is.shiny.appobj(app_dir)) {
    app_dir <- app_save(app_dir)
  }

  if (length(app_dir) == 1 && is.character(app_dir) && grepl("^http(s?)://", app_dir)) {
    private$shiny_url$set(app_dir)
    app_set_dir(self, private, ".")
  } else {
    "!DEBUG starting shiny app from path"
    self$log_message("Starting Shiny app")
    app_set_dir(self, private, app_dir)

    app_start_shiny(
      self, private,
      seed = seed,
      load_timeout = load_timeout,
      shiny_args = shiny_args,
      render_args = render_args,
      options = options
    )
  }

  # Read js content before init'ing chromote to reduce time between
  js_file <- system.file("internal", "js", "shiny-tracer.js", package = "shinytest2")
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
    {
      self$wait_for_js(
        "return window.shinytest2 && window.shinytest2.ready === true",
        timeout = load_timeout
      )
      self$wait_for_idle(duration = 500, timeout = load_timeout)
    },
    error = function(e) {
      rlang::abort(
        paste0(
          "Shiny app did not become stable in ", load_timeout, "ms.\n",
          "Message: ", conditionMessage(e)
        ),
        app = self,
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
  if (identical(private$shiny_worker_id, ""))
    private$shiny_worker_id <- NA_character_

  private$shiny_test_url <- chromote_eval(
    self$get_chromote_session(),
    "Shiny.shinyapp.getTestSnapshotBaseUrl ? Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl:true}) : null"
  )$result$value

  if (check_names) {
    "!DEBUG checking widget names"
    app_check_unique_widget_names(self, private)
  }

  invisible(self)
}


app_initialize <- function(self, private, ..., view = missing_arg()) {
  ckm8_assert_app_driver(self, private)

  withCallingHandlers(
    app_initialize_(self, private, ..., view = view),
    error = function(e) {
      withCallingHandlers(
        self$log_message(paste0("Error while initializing AppDriver:\n", conditionMessage(e))),
        error = function(ee) {
          rlang::inform(paste0("Could not log error message. Error: ", conditionMessage(ee)))
        }
      )

      # Open chromote session if it is not already open and `view != FALSE`
      # `view` defaults to `rlang::missing_arg()`
      withCallingHandlers(
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
            rlang::inform("`$view()`ing chromote session for debugging purposes")
            self$log_message("Viewing chromote session for debugging purposes")
            self$view()
          }
        },
        error = function(ee) {
          rlang::inform(c(
            "!" = paste0("Could not open chromote session. Error: ", conditionMessage(ee))
          ))
        }
      )

      logs <- withCallingHandlers(
        format(self$get_log()),
        error = function(e) "(Error retrieving logs)"
      )

      abort(
        c(
          conditionMessage(e),
          "\n",
          i = crayon::silver("You can inspect the failed AppDriver object via `rlang::last_error()$app`"),
          i = paste0("AppDriver logs:\n", logs),
          "\n"
        ),
        app = self,
        parent = e
      )
    }
  )
}
