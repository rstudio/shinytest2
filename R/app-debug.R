#' Debug log types
#'
#' All supported debug log types that are not `"all"` or `"none"`.
#'
#' @keywords internal
#' @export
debug_log_types <- function() {
  c(
    "shiny_console",
    "browser",
    "shinytest2"
  )
}


as_debug <- function(x) {
  x <- unique(x)
  checkmate::assert_subset(x, c(debug_log_types(), c("all", "none")), empty.ok = FALSE)

  if ("all" %in% x) {
    x <- debug_log_types()
  } else if ("none" %in% x) {
    x <- character(0)
  }

  x
}

make_shiny_console_log <- function(out, err) {
  out <- data.frame(
    stringsAsFactors = FALSE,
    level = if (length(out)) "INFO" else character(),
    timestamp = if (length(out)) as.POSIXct(NA) else as.POSIXct(character()),
    message = filter_log_text(out),
    type = if (length(out)) "shiny_console" else character()
  )
  err <- data.frame(
    stringsAsFactors = FALSE,
    level = if (length(err)) "ERROR" else character(),
    timestamp = if (length(err)) as.POSIXct(NA) else as.POSIXct(character()),
    message = filter_log_text(err),
    type = if (length(err)) "shiny_console" else character()
  )
  rbind(out, err)
}

make_browser_log <- function(log) {
  log$type <- if (nrow(log)) "browser" else character()
  log[, c("level", "timestamp", "message", "type")]
}

make_shinytest2_log <- function(entries) {
  data.frame(
    stringsAsFactors = FALSE,
    level = if (length(entries)) "INFO" else character(),
    timestamp = parsedate::parse_date(vapply(entries, "[[", "", "timestamp")),
    message = vapply(entries, "[[", "", "message"),
    type = if (length(entries)) "shinytest2" else character()
  )
}

merge_logs <- function(output) {
  log <- do.call(rbind, output)
  log <- log[order(log$timestamp), ]
  class(log) <- c("shinytest2_logs", class(log))
  log
}


# Remove problem characters from log text. Currently just "\f", which clears the
# console in RStudio.
filter_log_text <- function(str) {
  gsub("\f", "", str, fixed = TRUE)
}




#' @description
#' Query one or more of the debug logs.
#' @param type Log type: `"all"`, `"shiny_console"`, `"browser"`,
#'   or `"shinytest2"`.
#' @include shiny-driver.R
ShinyDriver2$set("public", "getDebugLog", function(type = c("all", debug_log_types())) {

  type <- as_debug(match.arg(type, several.ok = TRUE))

  output <- list()

  # It's possible for there not to be a shinyProcess object, if we're testing
  # against a remote server (as in shinyloadtest).
  if (!is.null(private$shinyProcess) && "shiny_console" %in% type) {
    "!DEBUG ShinyDriver2$getDebugLog shiny_console"
    out <- readLines(private$shinyProcess$get_output_file(), warn = FALSE)
    err <- readLines(private$shinyProcess$get_error_file(), warn = FALSE)
    output$shiny_console <- make_shiny_console_log(out = out, err = err)
  }

  if ("browser" %in% type) {
    "!DEBUG ShinyDriver2$getDebugLog browser"
    stop("TODO-barret; ShinyDriver2$getDebugLog(type = 'browser')")
    output$browser <- make_browser_log(private$web$readLog())
  }

  if ("shinytest2" %in% type) {
    "!DEBUG ShinyDriver2$getDebugLog shinytest2 log"
    output$shinytest <- make_shinytest2_log(self$chromote_session$executeScript(
      "if (! window.shinytest2) { return([]) }
      var res = window.shinytest2.log_entries;
      window.shinytest2.log_entries = [];
      return res;"
    ))
  }

  merge_logs(output)
})

#' @description
#' Enable/disable debugging messages
#' @param enable New value.
#' @include shiny-driver.R
ShinyDriver2$set("public", "enableDebugLogMessages", function(enable = TRUE) {
  self$chromote_session$executeScript(
    "window.shinytest2.log_messages = arguments[0]",
    enable
  )
  invisible(self)
})


#' @include shiny-driver.R
ShinyDriver2$set("private", "setupDebugging", function(debug) {
  "!DEBUG ShinyDriver2$setupDebugging"
  debug <- as_debug(debug)

  if (length(debug)) {
    ## TODO(-prev): poll the logs
  }
  invisible(self)
})
