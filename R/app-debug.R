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


obj_to_string <- function(obj) {
  switch(obj$type,
    "string" = obj$value,
    # There are other sub types that might be useful, but punting for now
    "object" = "[object Object]",
    {
      message("Unknown `obj_to_string()` type: ", obj$type)
      utils::str(obj)
      "(unknown)"
    }
  )
}
frames_to_msg <- function(details, url) {
  call_frames <- details$stackTrace$callFrames
  if (length(call_frames) == 0) return("")
  frames <- do.call(rbind, lapply(call_frames, as.data.frame))
  # Give anonymous functions a name
  frames$functionName[frames$functionName == ""] <- "(anonymous)" # nolint
  # Left justify the names
  frames$functionName <- format(frames$functionName) # nolint
  frames$url <- sub(url, "", frames$url)
  msg <-
    paste0(
      "    ", frames$functionName, " @ ", frames$url, ":", frames$lineNumber, ":", frames$columnNumber,
      collapse = "\n"
    )
  paste0("\n", msg)
}
exception_thrown_to_msg <- function(info, url) {
  exception_details <- info$exceptionDetails
  paste0(
    exception_details$text, " ", obj_to_string(exception_details$exception),
    frames_to_msg(exception_details, url)
  )
}
console_api_to_msg <- function(info, url) {
  args <- vapply(info$args, obj_to_string, character(1))

  paste0(
    "console.", info$type, "(\"", paste0(args, collapse = "\", \""), "\")",
    frames_to_msg(info, url)
  )
}


#' @include shiny-driver.R
ShinyDriver2$set("private", "browserLogs", list())

sd2_init_browser_debug <- function(self, private) {
  self$get_chromote_session()$Runtime$consoleAPICalled(function(info) {
    # message("Runtime.consoleAPICalled")
    msg <- console_api_to_msg(info, private$getShinyUrl())
    # TODO-barret; Should these messages be displayed in realtime?
    # cyan(msg)
    private$browserLogs[[length(private$browserLogs) + 1]] <-
      list(
        message = msg,
        level = switch(info$type, "error" = "ERROR", "INFO"),
        timestamp = Sys.time()
      )
  })
  self$get_chromote_session()$Runtime$exceptionThrown(function(info) {
    # message("Runtime.exceptionThrown")
    msg <- exception_thrown_to_msg(info, private$getShinyUrl())
    # TODO-barret; Should these messages be displayed in realtime?
    # cyan(msg)
    private$browserLogs[[length(private$browserLogs) + 1]] <-
      list(
        message = msg,
        level = "ERROR",
        timestamp = Sys.time()
      )
  })
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
  if (length(log) == 0) return(NULL)
  log <- do.call(rbind, lapply(log, as.data.frame))
  log$type <- "browser"
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
ShinyDriver2$set("public", "get_debug_log", function(type = c("all", debug_log_types())) {

  type <- as_debug(match.arg(type, several.ok = TRUE))

  output <- list()

  # It's possible for there not to be a shinyProcess object, if we're testing
  # against a remote server (as in shinyloadtest).
  if (!is.null(private$shinyProcess) && "shiny_console" %in% type) {
    "!DEBUG ShinyDriver2$get_debug_log shiny_console"
    out <- readLines(private$shinyProcess$get_output_file(), warn = FALSE)
    err <- readLines(private$shinyProcess$get_error_file(), warn = FALSE)
    output$shiny_console <- make_shiny_console_log(out = out, err = err)
  }

  if ("browser" %in% type) {
    "!DEBUG ShinyDriver2$get_debug_log browser"
    output$browser <- make_browser_log(private$browserLogs)
  }

  if ("shinytest2" %in% type) {
    "!DEBUG ShinyDriver2$get_debug_log shinytest2 log"
    output$shinytest <- make_shinytest2_log(self$execute_script(
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
ShinyDriver2$set("public", "enable_debug_log_messages", function(enable = TRUE) {
  self$execute_script(
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
