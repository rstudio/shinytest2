#' Start run_app as background process
#'
#' Required by \link{start_r_bg}.
#'
#' @param path Shiny app path.
#' @param port On which port to start the app.
#'
#' @keywords internal
shiny_bg <- function(path, port) {
  # main app
  options(shiny.port = port)
  shiny::runApp(path)
}

#' Start shinyloadtest recorder in the background
#'
#' Required by \link{start_r_bg}.
#'
#' @param port Port where runs the shiny apps. See \link{shiny_bg}.
#'
#' @keywords internal
recorder_bg <- function(port) {
  shinyloadtest::record_session(
    target_app_url = sprintf("http://127.0.0.1:%s", port),
    host = "127.0.0.1",
    port = 8600,
    output_file = "recording.log",
    open_browser = FALSE
  )
}

#' Start background R process
#'
#' Start process in the background. Required by
#' \link{record_loadtest}.
#'
#' @param fun Passed to \link[callr]{r_bg}.
#' @param path Where the Shiny app is located.
#' @param port Port on which runs the Shiny app.
#'
#' @return Process or error
#' @keywords internal
start_r_bg <- function(fun, path = NULL, port = 3515) {

  # remove NULL elements
  args <- Filter(Negate(is.null), list(path = path, port = port))

  process <- callr::r_bg(
    func = fun,
    args = args,
    stderr= "",
    stdout = ""
  )

  while (any(is.na(pingr::ping_port("127.0.0.1", 3515)))) {
    message("Waiting for Shiny app to start...")
    Sys.sleep(0.1)
  }

  attempt::stop_if_not(
    process$is_alive(),
    msg = "Unable to launch the subprocess"
  )

  process
}

#' Record a loadtest
#'
#' This maybe added to a GitHub actions workflow.
#'
#' @param path Shiny app path.
#' @param timeout Time to wait before starting the app.
#' @param workers How many parallel session to simulate during
#' the loadtest.
#'
#' @return A load test report in the public folder. This is then
#' published on GitHub page if added to a GA workflow.
#' @export
record_loadtest <- function(path, timeout = 15, workers = 5) {
  message("\n---- BEGIN LOAD-TEST ---- \n")
  # start app + recorder
  target <- start_r_bg(shiny_bg, path = path)
  recorder <- start_r_bg(recorder_bg)

  # start headless chrome (points to recorder!).
  # AppDriver also support remote urls.
  chrome <- shinytest2::AppDriver$new(
    "http://127.0.0.1:8600",
    load_timeout = timeout * 1000
  )

  chrome$set_inputs(mu = 4, timeout_ = timeout * 1000)

  # clean
  chrome$stop()
  # needed to avoid
  # java.lang.IllegalStateException: last event in log not a
  # WS_CLOSE (did you close the tab after recording?)
  Sys.sleep(2)

  # shinycannon (maybe expose other params later ...)
  target_url <- "http://127.0.0.1:3515"
  system(
    sprintf(
      "shinycannon recording.log %s --workers %s --loaded-duration-minutes 2 --output-dir run1",
      target_url, workers
    )
  )

  target$kill()

  # Treat data and generate report
  df <- shinyloadtest::load_runs("run1")
  shinyloadtest::shinyloadtest_report(
    df,
    "public/index.html",
    self_contained = TRUE,
    open_browser = FALSE
  )
}
