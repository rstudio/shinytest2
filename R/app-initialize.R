#' @include shiny-driver.R
ShinyDriver2$set("private", "name",    NULL) # character / NULL
ShinyDriver2$set("private", "variant", NULL) # character / NULL
ShinyDriver2$set("private", "state", "stopped") # stopped or running
ShinyDriver2$set("private", "shinyTestUrl", NULL) # URL for shiny's test API
ShinyDriver2$set("private", "shinyWorkerId", NA_character_)




#' @param path Path to a directory containing a Shiny app, i.e. a
#'   single `app.R` file or a `server.R`-`ui.R` pair.
#' @param load_timeout How long to wait for the app to load, in ms.
#'   This includes the time to start R. Defaults to 5s when running
#'   locally and 10s when running on CI. Maximum value is 10s.
#' @param screenshot Take screenshots for each snapshot?
# ' @param phantomTimeout How long to wait when connecting to phantomJS
# '  process, in ms
#' @template variant
#' @param check_names Check if widget names are unique?
#' @param debug Start the app in debugging mode? In debugging mode debug
#'   messages are printed to the console.
#' @param seed An optional random seed to use before starting the application.
#'   For apps that use R's random number generator, this can make their
#'   behavior repeatable.
#' @param clean_logs Whether to remove the stdout and stderr logs when the
#'   Shiny process object is garbage collected.
#' @param shiny_args A list of options to pass to [shiny::runApp()].
#' @param render_args Passed to `rmarkdown::run()` for interactive `.Rmd`s.
#' @param options A list of [base::options()] to set in the driver's child
#'   process.
#' @importFrom callr process
#' @importFrom rlang abort
#' @include shiny-driver.R
ShinyDriver2$set("public", "initialize", function(
  path = ".",
  ...,
  load_timeout = NULL,
  screenshot = TRUE,
  check_names = TRUE,
  name = NULL,
  variant = getOption("shinytest2.variant", os_name_and_r_version()),
  debug = c("none", "all", debug_log_types()),
  # phantomTimeout = 5000,
  seed = NULL,
  clean_logs = TRUE,
  shiny_args = list(),
  render_args = NULL,
  options = list()
) {
  ellipsis::check_dots_empty()

  private$snapshotScreenshot <- screenshot # nolint
  private$variant <- variant
  private$tempAppshotDir <- temp_file() # nolint
  private$snapshotCount <- Count$new() # nolint
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
  private$cleanLogs <- isTRUE(clean_logs) # nolint
  if (is.null(load_timeout)) {
    load_timeout <- if (on_ci()) 10000 else 5000
  }

  self$logEvent("Start ShinyDriver2 initialization")

  # if (is.null(find_phantom())) {
  #   abort("PhantomJS not found.")
  # }

  # "!DEBUG get phantom port (starts phantom if not running)"
  # self$logEvent("Getting PhantomJS port")
  # private$phantomPort <- get_phantomPort(timeout = phantomTimeout)

  if (shiny::is.shiny.appobj(path)) {
    path <- app_save(path)
  }

  if (grepl("^http(s?)://", path)) {
    private$setShinyUrl(path)
  } else {
    "!DEBUG starting shiny app from path"
    self$logEvent("Starting Shiny app")
    private$startShiny(path, seed, load_timeout, shiny_args, render_args, options)
  }

  # Read js content before init'ing chromote to reduce time between
  js_file <- system.file("internal", "js", "shiny-tracer.js", package = "shinytest2")
  js_content <- read_utf8(js_file)


  "!DEBUG create new phantomjs session"
  self$logEvent("Creating new phantomjs session")
  # private$web <- Session$new(port = private$phantomPort)
  private$chromote_session <- chromote::ChromoteSession$new()

  sd2_init_browser_debug(self, private)

  # ## Set implicit timeout to zero. According to the standard it should
  # ## be zero, but phantomjs uses about 200 ms
  # private$web$setTimeout(implicit = 0)


  "!DEBUG navigate to Shiny app"
  self$logEvent("Navigating to Shiny app")
  # private$web$go(private$getShinyUrl())
  self$get_chromote_session()$Page$navigate(private$getShinyUrl())

  # This feels like it is too late. There is no guarantee that the script will load in time.
  "!DEBUG inject shiny-tracer.js to load before all other scripts"
  self$logEvent("Injecting shiny-tracer.js")
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
  #       format(self$getDebugLog())
  #     ))
  #   }

  #   Sys.sleep(0.1)
  # }
  # self$get_chromote_session()$Page$loadEventFired()


  "!DEBUG wait until Shiny starts"
  self$logEvent("Waiting until Shiny app starts")
  # load_ok <- private$web$waitFor(
  #   'window.shinytest2 && window.shinytest2.ready === true',
  #   timeout = load_timeout
  # )
  # if (!load_ok) {
  #   abort(paste0(
  #     "Shiny app did not load in ", load_timeout, "ms.\n",
  #     format(self$getDebugLog())
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
      format(self$getDebugLog())
    ))
  }

  "!DEBUG shiny started"
  self$logEvent("Shiny app started")
  private$state <- "running"

  private$setupDebugging(debug)


  # private$shinyWorkerId <- private$web$executeScript(
  #   'return Shiny.shinyapp.config.workerId'
  # )
  private$shinyWorkerId <- chromote_eval( # nolint
    self$get_chromote_session(),
    "Shiny.shinyapp.config.workerId"
  )$result$value
  if (identical(private$shinyWorkerId, ""))
    private$shinyWorkerId <- NA_character_ # nolint
  # private$shinyTestSnapshotBaseUrl <- private$web$executeScript(
  #   'if (Shiny.shinyapp.getTestSnapshotBaseUrl)
  #     return Shiny.shinyapp.getTestSnapshotBaseUrl({ fullUrl:true });
  #   else
  #     return null;'
  # )
  private$shinyTestUrl <- chromote_eval( # nolint
    self$get_chromote_session(),
    "Shiny.shinyapp.getTestSnapshotBaseUrl ? Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl:true}) : null"
  )$result$value

  "!DEBUG checking widget names"
  if (check_names) self$checkUniqueWidgetNames()

  invisible(self)
})


# #' @description
# #' Download a snapshot. Generally, you should not call this function
# #' yourself; it will be generated by `record_test()` as needed.
# #' @param path Directory to save snapshots.
# #' @param screenshot Take screenshots for each snapshot?
# ShinyDriver2$set("public", "snapshotInit", function(path, screenshot = TRUE) {
#   if (grepl("^/", path)) {
#     abort("Snapshot dir must be a relative path.")
#   }

#   # Strip off trailing slash if present
#   path <- sub("/$", "", path)

#   private$snapshotCount <- Count$new()
#   private$snapshotDir <- path
#   private$snapshotScreenshot <- screenshot
#   self
# })
