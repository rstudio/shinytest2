app_initialize <- function(
  self, private,
  path = testthat::test_path("../../"),
  ...,
  load_timeout = NULL,
  screenshot_args = NULL,
  check_names = TRUE,
  name = NULL,
  variant = getOption("shinytest2.variant", platform_variant()),
  debug = FALSE,
  view = FALSE,
  seed = NULL,
  clean_logs = TRUE,
  shiny_args = list(),
  render_args = NULL,
  options = list()
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  private$path <- fs::path_abs(path)
  private$default_screenshot_args <- screenshot_args
  private$variant <- if (identical(variant, FALSE)) NULL else variant
  private$appshot_dir <- temp_file()
  private$appshot_count <- Count$new()
  private$shiny_url <- Url$new()
  private$debug <- isTRUE(as.logical(debug))
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
    load_timeout <- if (on_ci()) 10000 else 5000
  }

  self$log_message("Start AppDriver initialization")

  # if (is.null(find_phantom())) {
  #   abort("PhantomJS not found.")
  # }

  # "!DEBUG get phantom port (starts phantom if not running)"
  # self$log_message("Getting PhantomJS port")
  # private$phantomPort <- get_phantomPort(timeout = phantomTimeout)

  if (shiny::is.shiny.appobj(path)) {
    path <- app_save(path)
  }

  if (grepl("^http(s?)://", path)) {
    private$shiny_url$set(path)
  } else {
    "!DEBUG starting shiny app from path"
    self$log_message("Starting Shiny app")
    app_start_shiny(self, private, path, seed, load_timeout, shiny_args, render_args, options)
  }

  # Read js content before init'ing chromote to reduce time between
  js_file <- system.file("internal", "js", "shiny-tracer.js", package = "shinytest2")
  js_content <- read_utf8(js_file)


  "!DEBUG create new phantomjs session"
  self$log_message("Creating new chromote session")
  # private$web <- Session$new(port = private$phantomPort)
  private$chromote_session <- chromote::ChromoteSession$new()

  if (isTRUE(view)) {
    self$view()
  }

  app_init_browser_debug(self, private)

  # ## Set implicit timeout to zero. According to the standard it should
  # ## be zero, but phantomjs uses about 200 ms
  # private$web$setTimeout(implicit = 0)


  "!DEBUG navigate to Shiny app"
  self$log_message("Navigating to Shiny app")
  # private$web$go(private$shiny_url$get())
  self$get_chromote_session()$Page$navigate(private$shiny_url$get())

  # This feels like it is too late. There is no guarantee that the script will load in time.
  "!DEBUG inject shiny-tracer.js to load before all other scripts"
  self$log_message("Injecting shiny-tracer.js")
  # private$web$executeScript(js_content)
  # js_id <- self$get_chromote_session()$Page$addScriptToEvaluateOnNewDocument(js_file)
  # js_id <- self$get_chromote_session()$Page$addScriptToEvaluateOnNewDocument("https://cdnjs.cloudflare.com/ajax/libs/react-is/18.0.0-alpha-fd5e01c2e-20210913/umd/react-is.production.min.js")
  # print(list(js_id = js_id))
  utils::capture.output({
    chromote_eval(
      self$get_chromote_session(),
      js_content
    )
  })


  "!DEBUG wait for navigation to happen"
  # nav_stop_time <- as.numeric(Sys.time()) + load_timeout
  # while(identical(private$web$getUrl(), "about:blank")) {
  #   if (as.numeric(Sys.time()) > nav_stop_time) {
  #     abort(paste0(
  #       "Failed to navigate to Shiny app in ", load_timeout, "ms.\n",
  #       format(self$get_debug_log())
  #     ))
  #   }

  #   Sys.sleep(0.1)
  # }
  # self$get_chromote_session()$Page$loadEventFired()


  "!DEBUG wait until Shiny starts"
  self$log_message("Waiting until Shiny app starts")
  # load_ok <- private$web$waitFor(
  #   'window.shinytest2 && window.shinytest2.ready === true',
  #   timeout = load_timeout
  # )
  # if (!load_ok) {
  #   abort(paste0(
  #     "Shiny app did not load in ", load_timeout, "ms.\n",
  #     format(self$get_debug_log())
  #   ))
  # }

# self$get_chromote_session()$view()
  # browser()

  if (!isTRUE(chromote_wait_for_condition(
    self$get_chromote_session(),
    "window.shinytest2 && window.shinytest2.ready === true",
    timeout = load_timeout
  ))) {
    abort(paste0(
      "Shiny app did not load in ", load_timeout, "ms.\n",
      format(self$get_debug_log())
    ))
  }

  "!DEBUG shiny started"
  self$log_message("Shiny app started")
  private$state <- "running"

  # private$setup_debugging(debug)


  # private$shiny_worker_id <- private$web$executeScript(
  #   'return Shiny.shinyapp.config.workerId'
  # )
  private$shiny_worker_id <- chromote_eval(
    self$get_chromote_session(),
    "Shiny.shinyapp.config.workerId"
  )$result$value
  if (identical(private$shiny_worker_id, ""))
    private$shiny_worker_id <- NA_character_
  # private$shinyTestSnapshotBaseUrl <- private$web$executeScript(
  #   'if (Shiny.shinyapp.getTestSnapshotBaseUrl)
  #     return Shiny.shinyapp.getTestSnapshotBaseUrl({ fullUrl:true });
  #   else
  #     return null;'
  # )
  private$shiny_test_url <- chromote_eval(
    self$get_chromote_session(),
    "Shiny.shinyapp.getTestSnapshotBaseUrl ? Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl:true}) : null"
  )$result$value

  "!DEBUG checking widget names"
  if (check_names) app_check_unique_widget_names(self, private)

  invisible(self)
}
