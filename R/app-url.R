#' @include shiny-driver.R
# TODO-barret; Why do these values exist when we have `public$getUrl()`? They should all be removed in favor of a single `$shinyURL` field
ShinyDriver2$set("private", "shinyUrlProtocol", NULL) # "http" or "https"
ShinyDriver2$set("private", "shinyUrlHost",     NULL) # usually 127.0.0.1
ShinyDriver2$set("private", "shinyUrlPort",     NULL)
ShinyDriver2$set("private", "shinyUrlPath",     NULL)


#' @include shiny-driver.R
ShinyDriver2$set("private", "getShinyUrl", function() {
  paste0(
    private$shinyUrlProtocol, "://", private$shinyUrlHost,
    if (!is.null(private$shinyUrlPort)) paste0(":", private$shinyUrlPort),
    private$shinyUrlPath
  )
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

  # nolint start
  private$shinyUrlProtocol <- res$protocol
  private$shinyUrlHost     <- res$host
  private$shinyUrlPort     <- res$port
  private$shinyUrlPath     <- res$path
  # nolint end

  invisible(self)
})
