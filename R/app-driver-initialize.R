app_initialize_ <- function(
  self, private,
  path = testthat::test_path("../../"),
  ...,
  load_timeout = NULL,
  screenshot_args = NULL,
  check_names = TRUE,
  name = NULL,
  variant = getOption("shinytest2.variant", platform_variant()),
  view = missing_arg(),
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

  private$path <- fs::path_abs(path)
  private$default_screenshot_args <- screenshot_args
  private$variant <- if (identical(variant, FALSE)) NULL else variant
  private$appshot_dir <- temp_file()
  private$appshot_count <- Count$new()
  private$shiny_url <- Url$new()
  private$name <-
    if (!is.null(name)) {
      name
    } else {
      full_path <- fs::path_abs(path)
      if (fs::dir_exists(full_path)) {
        # get folder name
        fs::path_file(full_path)
      } else {
        # get parent folder name
        fs::path_file(
          fs::path_dir(full_path)
        )
      }
    }
  private$clean_logs <- isTRUE(clean_logs)
  if (is.null(load_timeout)) {
    load_timeout <- if (on_ci()) 20 * 1000 else 10 * 1000
  }

  self$log_message("Start AppDriver initialization")

  if (shiny::is.shiny.appobj(path)) {
    path <- app_save(path)
  }

  private$state <- "initialize"

  if (grepl("^http(s?)://", path)) {
    private$shiny_url$set(path)
  } else {
    "!DEBUG starting shiny app from path"
    self$log_message("Starting Shiny app")
    app_start_shiny(
      self, private,
      path = path,
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
  self$log_message("Creating new chromote session")
  private$chromote_session <- chromote::ChromoteSession$new()

  if (isTRUE(view)) {
    self$view()
  }

  app_init_browser_log(self, private, options = options)

  "!DEBUG navigate to Shiny app"
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

  rlang::with_handlers(
    {
      chromote_wait_for_condition(
        self$get_chromote_session(),
        "window.shinytest2 && window.shinytest2.ready === true",
        timeout = load_timeout
      )
      self$wait_for_stable(timeout = load_timeout)
    },
    error = function(e) {
      abort(
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


app_initialize <- function(self, private, ...) {
  ckm8_assert_app_driver(self, private)

  rlang::with_handlers(
    app_initialize_(self, private, ...),
    error = function(e) {
      rlang::with_handlers(
        self$log_message(paste0("Error while initializing AppDriver:\n", conditionMessage(e))),
        error = function(ee) {
          message("Could not log error message. Error: ", conditionMessage(ee))
        }
      )

      # Open chromote session if it is not already open and `view != FALSE`
      # `view` defaults to `rlang::missing_arg()`
      rlang::with_handlers(
        {
          view_val <- list(...)$view
          if (rlang::is_interactive() && !isTRUE(view_val) && !is_false(view_val)) {
            message("`$view()`ing chromote session for debugging purposes")
            self$log_message("Viewing chromote session for debugging purposes")
            self$view()
          }
        },
        error = function(ee) {
          message("Could not open chromote session. Error: ", conditionMessage(ee))
        }
      )

      logs <- rlang::with_handlers(
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
