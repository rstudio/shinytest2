#' @export
#' @importFrom crayon blue magenta cyan green red silver make_style
format.shinytest2_log <- function(x, ...) {

  get_color <- function(location, level) {
    switch(location,
      shiny = switch(level, error = magenta, force),
      chromote = switch(level, throw = , exception = , error = red, cyan),
      shinytest2 = switch(level, green)
    )
  }

  types <- c(
    chromote = "{chromote}",
    shinytest2 = "{shinytest2}",
    shiny = "{shiny}"
  )
  language <- c(
    chromote = "JS",
    shinytest2 = "R",
    shiny = "R"
  )

  x[[".type"]] <- types[x$location]
  x[[".language"]] <- language[x$location]
  x[[".msg"]] <- Map(x$message, Map(x$location, x$level, f = get_color), f = function(msg, color) {
    color(msg)
  })

  x[[".timestamp"]] <- vapply(x$timestamp, function(timestamp) {
    if (is.na(timestamp)) "-----------"
    else format(timestamp, "%H:%M:%OS2")
  }, character(1))

  if (inherits(x, "shinytest2_log_grouped")) {
    type_names <- c(
      shiny = "{shiny} R console output",
      chromote = "{chromote} JavaScript console output",
      shinytest2 = "{shinytest2} log entries"
    )
    x[[".type_names"]] <- type_names[x$location]

    x_groups <- split(x, x$location)

    fmt_groups <-
      lapply(names(type_names), function(loc) {

        x_loc <- x_groups[[loc]]

        log_level <- switch(loc,
          "shinytest2" = "",
          paste0(x_loc$level, " ")
        )

        msg <-
        switch(loc,
          "shiny" = paste0(x_loc[[".msg"]], collapse = "\n"),
          paste0(
            log_level, x_loc[[".timestamp"]], " ", x_loc[[".msg"]],
            collapse = "\n"
          )
        )

        paste0(
          type_names[loc], "\n",
          paste0(rep("=", nchar(type_names[loc])), collapse = ""), "\n",
          msg
        )
      })

    # Group with two blank lines between each group
    paste0(unlist(fmt_groups), collapse = "\n\n\n")

  } else if (inherits(x, "shinytest2_log_ungrouped")) {
    x[[".identifier"]] <- paste0(format(x[[".type"]]), " ", format(x[[".language"]]), " ", format(x$level))

    first_msg <- paste0(
      x[[".identifier"]], " ", x[[".timestamp"]], " "
    )
    first_msg_char_len <- nchar(first_msg[1])
    first_spaces <- paste0("\n", paste0(rep(" ", first_msg_char_len), collapse = ""))
    msg <- gsub("\n", first_spaces, x[[".msg"]], fixed = TRUE)
    paste0(
      first_msg, msg,
      collapse = "\n"
    )

  } else {
    stop("Missing group class from shinytest2 log")
  }


}

#' @export
print.shinytest2_log <- function(x, ...) {
  cat(format(x), ...)
  invisible(x)
}
