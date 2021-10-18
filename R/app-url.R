#' @include shiny-driver.R
ShinyDriver2$set("private", "shinyUrl", NULL)


#' @include shiny-driver.R
ShinyDriver2$set("private", "getShinyUrl", function() {
  private$shinyUrl
})

#' @include shiny-driver.R
ShinyDriver2$set("private", "setShinyUrl", function(url) {
  res <- parse_url(url)

  if (nzchar(res$port)) {
    res$port <- as.integer(res$port)
    ckm8_assert_single_integer(res$port)
  } else {
    res$port <- NULL
  }

  res$path <- if (nzchar(res$path)) res$path else "/"

  ckm8_assert_single_string(res$host)
  ckm8_assert_single_url(res$path)

  private$shinyUrl <- paste0( # nolint
    res$protocol, "://", res$host,
    if (!is.null(res$port)) paste0(":", res$port),
    res$path
  )

  invisible(self)
})
