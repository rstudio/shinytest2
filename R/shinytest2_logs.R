#' @export
#' @importFrom crayon blue magenta cyan make_style
format.shinytest2_logs <- function(x, ..., short = FALSE) {

  colors <- list(
    shiny_console = magenta,
    browser = cyan,
    shinytest2 = blue
  )

  types <- c(
    shiny_console = "C",
    browser = "B",
    shinytest2 = "S"
  )

  lines <- vapply(seq_len(nrow(x)), function(i) {

    if (short) {
      return(
        paste0(
          types[x$type[i]], "> ",
          colors[[x$type[i]]](x$message[i])
        )
      )
    }

    time <- if (is.na(x$timestamp[i])) {
      "-----------"
    } else {
      format(x$timestamp[i], "%H:%M:%OS2")
    }

    paste(
      sep = "",
      types[x$type[i]],
      "/",
      substr(x$level[i], 1, 1),
      " ",
      time,
      " ",
      colors[[x$type[i]]](x$message[i])
    )
  }, character(1))

  paste(lines, collapse = "\n")
}

#' @export
#' @importFrom crayon blue magenta cyan make_style

print.shinytest2_logs <- function(x, ..., short = FALSE) {
  cat(format(x, short = short), ...)
  invisible(x)
}
