#' @importFrom callr process
#' @importFrom rlang abort

sd2_initialize <- function(
  self, private,
  path, loadTimeout, checkNames, name, variant,
  debug, phantomTimeout, seed, cleanLogs,
  shinyOptions, renderArgs, options
) {

  private$variant <- variant
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
  private$cleanLogs <- cleanLogs
  if (is.null(loadTimeout)) {
    loadTimeout <- if (on_ci()) 10000 else 5000
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
    private$startShiny(path, seed, loadTimeout, shinyOptions, renderArgs, options)
  }

  # Read js content before init'ing chromote to reduce time between
  js_file <- system.file("internal", "js", "shiny-tracer.js", package = "shinytest2")
  js_content <- read_utf8(js_file)


  "!DEBUG create new phantomjs session"
  self$logEvent("Creating new phantomjs session")
  # private$web <- Session$new(port = private$phantomPort)
  private$chromote_obj <- chromote::ChromoteSession$new()

  # ## Set implicit timeout to zero. According to the standard it should
  # ## be zero, but phantomjs uses about 200 ms
  # private$web$setTimeout(implicit = 0)


  "!DEBUG navigate to Shiny app"
  self$logEvent("Navigating to Shiny app")
  # private$web$go(private$getShinyUrl())
  private$chromote_obj$Page$navigate(private$getShinyUrl())

  # This feels like it is too late. There is no guarantee that the script will load in time.
  "!DEBUG inject shiny-tracer.js to load before all other scripts"
  self$logEvent("Injecting shiny-tracer.js")
  # private$web$executeScript(js_content)
  # js_id <- private$chromote_obj$Page$addScriptToEvaluateOnNewDocument(js_file)
  # js_id <- private$chromote_obj$Page$addScriptToEvaluateOnNewDocument("https://cdnjs.cloudflare.com/ajax/libs/react-is/18.0.0-alpha-fd5e01c2e-20210913/umd/react-is.production.min.js")
  # print(list(js_id = js_id))
  utils::capture.output({

    chromote_eval(
      private$chromote_obj,
      js_content
    )
  })


  "!DEBUG wait for navigation to happen"
  # nav_stop_time <- as.numeric(Sys.time()) + loadTimeout
  # while(identical(private$web$getUrl(), "about:blank")) {
  #   if (as.numeric(Sys.time()) > nav_stop_time) {
  #     abort(paste0(
  #       "Failed to navigate to Shiny app in ", loadTimeout, "ms.\n",
  #       format(self$getDebugLog())
  #     ))
  #   }

  #   Sys.sleep(0.1)
  # }
  # private$chromote_obj$Page$loadEventFired()


  "!DEBUG wait until Shiny starts"
  self$logEvent("Waiting until Shiny app starts")
  # load_ok <- private$web$waitFor(
  #   'window.shinytest2 && window.shinytest2.ready === true',
  #   timeout = loadTimeout
  # )
  # if (!load_ok) {
  #   abort(paste0(
  #     "Shiny app did not load in ", loadTimeout, "ms.\n",
  #     format(self$getDebugLog())
  #   ))
  # }

  # browser()

  if (is.na(chromote_wait_for_condition(
    private$chromote_obj,
    "window.shinytest2 && window.shinytest2.ready === true",
    timeout_ms = loadTimeout
  ))) {
    abort(paste0(
      "Shiny app did not load in ", loadTimeout, "ms.\n",
      NULL
      # TODO-barret
      # format(self$getDebugLog())
    ))
  }

  "!DEBUG shiny started"
  self$logEvent("Shiny app started")
  private$state <- "running"

  private$setupDebugging(debug)


  # private$shinyWorkerId <- private$web$executeScript(
  #   'return Shiny.shinyapp.config.workerId'
  # )
  private$shinyWorkerId <- chromote_eval(
    private$chromote_obj,
    "Shiny.shinyapp.config.workerId"
  )$result$value
  if (identical(private$shinyWorkerId, ""))
    private$shinyWorkerId <- NA_character_

  # private$shinyTestSnapshotBaseUrl <- private$web$executeScript(
  #   'if (Shiny.shinyapp.getTestSnapshotBaseUrl)
  #     return Shiny.shinyapp.getTestSnapshotBaseUrl({ fullUrl:true });
  #   else
  #     return null;'
  # )
  private$shinyTest2SnapshotBaseUrl <- chromote_eval(
    private$chromote_obj,
    "Shiny.shinyapp.getTestSnapshotBaseUrl ? Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl:true}) : null"
  )$result$value

  "!DEBUG checking widget names"
  if (checkNames) self$checkUniqueWidgetNames()

  invisible(self)
}


sd2_snapshotInitLegacy <- function(self, private, path, screenshot) {
  if (grepl("^/", path)) {
    abort("Snapshot dir must be a relative path.")
  }

  # Strip off trailing slash if present
  path <- sub("/$", "", path)

  private$snapshotCount <- 0
  private$snapshotDir <- path
  private$snapshotScreenshot <- screenshot
}
