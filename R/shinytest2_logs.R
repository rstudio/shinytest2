#' @export
#' @importFrom crayon blue magenta cyan green red silver make_style
format.shinytest2_log <- function(x, ...) {

  get_color <- function(location, level) {
    switch(location,
      shiny = switch(level, error = magenta, force),
      chromote = switch(level, throw = , error = red, cyan),
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
}

#' @export
print.shinytest2_log <- function(x, ...) {
  cat(format(x), ...)
  invisible(x)
}
