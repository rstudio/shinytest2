#' Debug log types
#'
#' All supported debug log types that are not `"all"` or `"none"`.
#'
#' There are a few standard debug types that may be used:
#' * `"shiny_console"`: Displays the console messages from the Shiny server when `$get_debug_log()` is called.
#' * `"browser"`: Displays the browser console messages when `$get_debug_log()` is called.
#' * `"shinytest2"`: Displays the messages saved by the `window.shinytest2` object in the browser when `$get_debug_log()` is called.
#' * `"ws_messages"`: Saves all messages sent by Shiny to the
#'
#' @keywords internal
#' @export
debug_types <- function(shiny_console = TRUE, browser = TRUE, shinytest2 = TRUE) {
  if (all(!shiny_console, !browser, !shinytest2)) {
    abort("At least one debug type must be specified.")
  }

  c(
    if (shiny_console) "shiny_console",
    if (browser) "browser",
    if (shinytest2) "shinytest2"
  )
}


obj_to_string <- function(obj) {
  switch(obj$type,
    "string" = obj$value,
    # TODO-future; There are other sub types that might be useful, but punting for now
    "object" = {
      if (obj$subtype == "error") {
        obj$description
      } else {
        "[object Object]"
      }
    },
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
  first_fn_name <- call_frames[[1]]$functionName
  if (
    details$type == "info" &&
    !is.null(first_fn_name) &&
    first_fn_name == "window.shinytest2.shinytest2.log"
  ) {
    return("")
  }

  frames <- do.call(rbind, lapply(call_frames, as.data.frame))
  # Give anonymous functions a name
  frames$functionName[frames$functionName == ""] <- "(anonymous)" # nolint
  # Left justify the names
  frames$functionName <- format(frames$functionName) # nolint
  frames$url <- sub(url, "", frames$url)
  msg <-
    paste0(
      # "    ",
      frames$functionName, " @ ", frames$url, ":", frames$lineNumber, ":", frames$columnNumber,
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
    # "console.", info$type, "(\"", paste0(args, collapse = "\", \""), "\")",
    # "console.", info$type, " - ", paste0(args, collapse = " "),
    paste0(args, collapse = " "),
    frames_to_msg(info, url)
  )
}


app_init_browser_debug <- function(self, private) {
  ckm8_assert_app_driver(self, private)

  self$get_chromote_session()$Runtime$consoleAPICalled(function(info) {
    # message("Runtime.consoleAPICalled")
    msg <- console_api_to_msg(info, private$shiny_url$get())

    entry <- app_add_debug_log_entry(
      self,
      private,
      location = "chromote",
      level = info$type,
      message = msg
    )
    if (private$debug) {
      print.shinytest2_log(entry)
    }
  })
  self$get_chromote_session()$Runtime$exceptionThrown(function(info) {
    # message("Runtime.exceptionThrown")
    msg <- exception_thrown_to_msg(info, private$shiny_url$get())
    entry <- app_add_debug_log_entry(
      self,
      private,
      location = "chromote",
      level = "throw",
      message = msg
    )
    if (private$debug) {
      print.shinytest2_log(entry)
    }
  })
}




# as_debug <- function(x) {
#   x <- unique(x)
#   checkmate::assert_subset(x, c(debug_types(), c("all", "none")), empty.ok = FALSE)

#   if ("all" %in% x) {
#     x <- debug_types()
#   } else if ("none" %in% x) {
#     x <- character(0)
#   }

#   x
# }

app_make_shiny_log <- function(self, private, out, err) {
  out <- readLines(private$shiny_process$get_output_file(), warn = FALSE)
  err <- readLines(private$shiny_process$get_error_file(), warn = FALSE)

  c(
    lapply(out, function(out_val) {
      app_debug_log_entry(
        self,
        private,
        location = "shiny",
        level = "info",
        message = filter_log_text(out_val),
        timestamp = as.POSIXct(NA)
      )
    }),
    lapply(err, function(err_val) {
      app_debug_log_entry(
        self,
        private,
        location = "shiny",
        level = "error",
        message = filter_log_text(err_val),
        timestamp = as.POSIXct(NA)
      )
    })
  )
}

# app_make_browser_log <- function(self, private) {
#   chm8_assert_app_driver(self, private)

#   log <- private$browser_log
#   if (length(log) == 0) return(NULL)
#   log <- do.call(rbind, lapply(log, as.data.frame))
#   log$type <- "browser"
#   log[, c("level", "timestamp", "message", "type")]
#   app_debug_log_entry
# }

# app_make_shinytest2_log <- function(self, private, entries) {
#   ckm8_assert_app_driver(self, private)

#   entries <-
#     self$execute_script(
#       "if (! window.shinytest2) { return([]) }
#       var res = window.shinytest2.log_entries;
#       // window.shinytest2.log_entries = [];
#       return res;"
#     )

#   # data.frame(
#   #   stringsAsFactors = FALSE,
#   #   level = if (length(entries)) "INFO" else character(),
#   #   timestamp = parsedate::parse_date(vapply(entries, "[[", "", "timestamp")),
#   #   message = vapply(entries, "[[", "", "message"),
#   #   type = if (length(entries)) "shinytest2" else character()
#   # )
#   lapply(entries, function(entry) {
#     app_debug_log_entry(
#       self, private,
#       location = "shinytest2",
#       level = "info",
#       message = entry$message,
#       timestamp = parsedate::parse_date(entry$time),
#       data = NULL
#     )
#   })
# }



# Remove problem characters from log text. Currently just "\f", which clears the
# console in RStudio.
filter_log_text <- function(str) {
  gsub("\f", "", str, fixed = TRUE)
}



app_get_debug_log <- function(
  self, private
) {
  ckm8_assert_app_driver(self, private)

  log <- do.call(rbind, c(
    # browser console / shinytest2 + manual entries
    private$debug_log,
    # shiny console
    app_make_shiny_log(self, private)
  ))

  log <- log[order(log$timestamp), ]
  class(log) <- c("shinytest2_log", class(log))
  log
}
