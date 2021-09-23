sd2_startShiny <- function(self, private, path, seed, loadTimeout, shinyOptions, renderArgs, options) {
  ckm8_assert_single_string(path)

  private$path <- normalizePath(path)

  if (is.null(shinyOptions$port)) {
    shinyOptions$port <- httpuv::randomPort()
  }

  tempfile_format <- tempfile("%s-", fileext = ".log")

  # the RNG kind should inherit from the parent process
  rng_kind <- RNGkind()

  p <- withr::with_envvar(
    c("R_TESTS" = NA),
    callr::r_bg(
      function(path, shinyOptions, rmd, seed, rng_kind, renderArgs, options) {

        if (!is.null(seed)) {
          # Prior to R 3.6, RNGkind has 2 args, otherwise it has 3
          do.call(RNGkind, as.list(rng_kind))
          set.seed(seed);
          getNamespace("shiny")$withPrivateSeed(set.seed(seed + 11))
        }

        options <- as.list(options)
        options$shiny.testmode <- TRUE
        do.call(base::options, options)

        if (rmd) {
          # Shiny document
          rmarkdown::run(path, shiny_args = shinyOptions, render_args = renderArgs)
        } else {
          # Normal shiny app
          do.call(shiny::runApp, c(path, shinyOptions))
        }
      },
      args = list(
        path = path,
        shinyOptions = shinyOptions,
        rmd = is_rmd(path),
        seed = seed,
        rng_kind = rng_kind,
        renderArgs = renderArgs,
        options = options
      ),
      stdout = sprintf(tempfile_format, "shiny-stdout"),
      stderr = sprintf(tempfile_format, "shiny-stderr"),
      supervise = TRUE
    )
  )
  "!DEBUG waiting for shiny to start"
  if (! p$is_alive()) {
    abort(paste0(
      "Failed to start shiny. Error: ",
      strwrap(readLines(p$get_error_file()))
    ))
  }

  "!DEBUG finding shiny port"
  ## Try to read out the port. Try 5 times/sec, until timeout.
  max_i <- loadTimeout / 1000 * 5
  for (i in seq_len(max_i)) {
    err_lines <- readLines(p$get_error_file())

    if (!p$is_alive()) {
      abort(paste0(
        "Error starting application:\n", paste(err_lines, collapse = "\n")
      ))
    }
    if (any(grepl("Listening on http", err_lines))) break

    Sys.sleep(0.2)

    if (i == max_i) {
      abort(paste0(
        "Cannot find shiny port number. Error:\n", paste(err_lines, collapse = "\n")
      ))
    }
  }

  line <- err_lines[grepl("Listening on http", err_lines)]

  # m[, 'port'] should be the same as port, but we don't enforce it.
  m <- rematch::re_match(text = line, "https?://(?<host>[^:]+):(?<port>[0-9]+)")
  "!DEBUG shiny up and running, port `m[, 'port']`"

  url <- sub(".*(https?://.*)", "\\1", line)
  private$setShinyUrl(url)

  private$shinyProcess <- p
}


sd2_getShinyUrl <- function(self, private) {
  paste0(
    private$shinyUrlProtocol, "://", private$shinyUrlHost,
    if (!is.null(private$shinyUrlPort)) paste0(":", private$shinyUrlPort),
    private$shinyUrlPath
  )
}

sd2_setShinyUrl <- function(self, private, url) {
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

  private$shinyUrlProtocol <- res$protocol
  private$shinyUrlHost     <- res$host
  private$shinyUrlPort     <- res$port
  private$shinyUrlPath     <- res$path
}
