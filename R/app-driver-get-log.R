

obj_to_string <- function(obj) {
  switch(obj$type,
    "boolean" = if (obj$value) "true" else "false",
    "string" = obj$value,
    "number" = as.character(obj$value),
    "symbol" = , # nolint
    "function" = {
      obj$description
    },
    "undefined" = "undefined",
    "bigint" = {
      obj$unserializableValue
    },
    "object" = {
      switch(obj$subtype %||% "unknown",
        "error" = {
          obj$description
        },
        "array" = {
          vals <- vapply(obj$preview$properties, obj_to_string, character(1))
          if (isTRUE(obj$preview$overflow)) {
            vals[length(vals) + 1] <- "..."
          }
          paste0("[", paste0(vals, collapse = ","), "]")
        },
        "null" = "null",
        {
          # Work with objects like `Math`
          paste0("[object", " ", obj$className %||% "Object", "]")
        }
      )
    },
    {
      structure <- paste0(
        # Try to caputre some, but not all output
        utils::capture.output(utils::str(obj, max.level = 4)),
        collapse = "\n"
      )
      rlang::inform(
        c(
          "!" = paste0("Unknown JavaScript return type: ", obj$type),
          "*" = paste0("Structure:\n", structure),
          ">" = "Please report this new type with an example structure on https://github.com/rstudio/shinytest2/issues/new"
        ),
        .frequency = "once",
        .frequency_id = obj$type
      )
      "(unknown)"
    }
  )
}
frames_to_msg <- function(details, url) {
  call_frames <- details$stackTrace$callFrames
  if (length(call_frames) == 0) {
    return("")
  }
  if (length(details$type) == 1 && details$type == "info") {
    first_fn_name <- call_frames[[1]]$functionName
    if (first_fn_name == "window.shinytest2.shinytest2.log") {
      return("")
    }
  }

  frames <- do.call(rbind, lapply(call_frames, as.data.frame, stringsAsFactors = FALSE))
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


app_init_browser_log <- function(self, private, options) {
  ckm8_assert_app_driver(self, private)

  self$get_chromote_session()$Runtime$consoleAPICalled(function(info) {
    # message("Runtime.consoleAPICalled")

    msg <- console_api_to_msg(info, private$shiny_url$get())

    app_add_log_entry(
      self,
      private,
      location = "chromote",
      level = info$type,
      message = msg
    )
    # # Display logs in realtime
    # if (private$debug_flag) {
    #   print.shinytest2_log(entry)
    # }
  })
  self$get_chromote_session()$Runtime$exceptionThrown(function(info) {
    # message("Runtime.exceptionThrown")

    msg <- exception_thrown_to_msg(info, private$shiny_url$get())
    app_add_log_entry(
      self,
      private,
      location = "chromote",
      level = "throw",
      message = msg
    )
    # # Display logs in realtime
    # if (private$debug_flag) {
    #   print.shinytest2_log(entry)
    # }
  })


  # Only display websocket traffic if `options = list(shiny.trace = TRUE)` is supplied on init
  if (isTRUE(options$shiny.trace)) {
      # Shorten all base64 encoded strings to `[base64 data]`
      # https://github.com/rstudio/shiny/blob/b52b9e4520ad8d1e976299d5dec5e4ba3096bd04/R/shiny.R#L400
    paste_and_shorten_base64 <- function(method, txt) {
      txt <- gsub(
        "(?m)base64,[a-zA-Z0-9+/=]+",
        "[base64 data]",
        txt,
        perl = TRUE
      )
      paste0(method, " ", txt)
    }
    self$get_chromote_session()$Network$webSocketFrameSent(function(info) {
      # >str(info)
      # List of 3
      # $ requestId: chr "34121.21"
      # $ timestamp: num 307537
      # $ response :List of 3
      #   ..$ opcode     : int 1
      #   ..$ mask       : logi TRUE
      #   ..$ payloadData: chr "{\"method\":\"init\",\"data| __truncated__

      app_add_log_entry(
        self, private,
        location = "chromote",
        level = "websocket",
        message = paste_and_shorten_base64("send", info$response$payloadData)
      )
    })
    self$get_chromote_session()$Network$webSocketFrameReceived(function(info) {
      # >str(info)
      # List of 3
      # $ requestId: chr "34121.21"
      # $ timestamp: num 307537
      # $ response :List of 3
      #   ..$ opcode     : int 1
      #   ..$ mask       : logi TRUE
      #   ..$ payloadData: chr "{\"method\":\"init\",\"data| __truncated__

      app_add_log_entry(
        self, private,
        location = "chromote",
        level = "websocket",
        message = paste_and_shorten_base64("recv", info$response$payloadData)
      )
    })
  }

}




app_make_shiny_log <- function(self, private, out, err) {
  if (is.null(private$shiny_process)) return(NULL)

  out <- readLines(private$shiny_process$get_output_file(), warn = FALSE)
  err <- readLines(private$shiny_process$get_error_file(), warn = FALSE)

  # Display errors first!
  c(
    lapply(err, function(err_val) {
      app_log_entry(
        self,
        private,
        location = "shiny",
        level = "stderr",
        message = filter_log_text(err_val),
        timestamp = as.POSIXct(NA)
      )
    }),
    lapply(out, function(out_val) {
      app_log_entry(
        self,
        private,
        location = "shiny",
        level = "stdout",
        message = filter_log_text(out_val),
        timestamp = as.POSIXct(NA)
      )
    })
  )
}
# Remove problem characters from log text. Currently just "\f", which clears the
# console in RStudio.
filter_log_text <- function(str) {
  gsub("\f", "", str, fixed = TRUE)
}



app_get_logs <- function(
  self, private
) {
  ckm8_assert_app_driver(self, private)

  logs <- do.call(rbind, c(
    # browser console / shinytest2 + manual entries
    private$logs,
    # shiny console
    app_make_shiny_log(self, private)
  ))

  logs <- logs[order(logs$timestamp), ]
  class(logs) <- c("shinytest2_log", class(logs))
  logs
}
