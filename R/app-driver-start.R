
app_start_shiny <- function(
  self, private,
  path,
  seed = NULL,
  load_timeout = 10000,
  shiny_args = list(),
  render_args = NULL,
  options = list()
) {
  ckm8_assert_app_driver(self, private)
  ckm8_assert_single_string(path)

  if (is.null(shiny_args$port)) {
    shiny_args$port <- httpuv::randomPort()
  }

  tempfile_format <- temp_file(pattern = "%s-", fileext = ".log")

  # the RNG kind should inherit from the parent process
  rng_kind <- RNGkind()

  p <- withr::with_envvar(
    c("R_TESTS" = NA),
    callr::r_bg(
      function(path, shiny_args, rmd, seed, rng_kind, render_args, options) {

        if (!is.null(seed)) {
          # Prior to R 3.6, RNGkind has 2 args, otherwise it has 3
          do.call(RNGkind, as.list(rng_kind))
          set.seed(seed);
          getNamespace("shiny")$withPrivateSeed(set.seed(seed + 11))
        }

        options <- as.list(options)
        options[["shiny.testmode"]] <- TRUE
        # TODO-barret-future; Adjust shiny to add htmldeps to the list of the rendered page
        # options[["shiny-testmode-html-dep"]] <- getTracerDep()
        do.call(base::options, options)

        if (rmd) {
          # Shiny document
          rmarkdown::run(path, shiny_args = shiny_args, render_args = render_args)
        } else {
          # Normal shiny app
          do.call(shiny::runApp, c(path, shiny_args))
        }
      },
      args = list(
        path = path,
        shiny_args = shiny_args,
        rmd = is_rmd(path),
        seed = seed,
        rng_kind = rng_kind,
        render_args = render_args,
        options = options
      ),
      stdout = sprintf(tempfile_format, "shiny-stdout"),
      stderr = sprintf(tempfile_format, "shiny-stderr"),
      supervise = TRUE
    )
  )
  private$shiny_process <- p # nolint

  "!DEBUG waiting for shiny to start"
  if (! p$is_alive()) {
    abort(paste0(
      "Failed to start shiny. Error:\n",
      strwrap(readLines(p$get_error_file()))
    ), app = self)
  }

  "!DEBUG finding shiny port"
  ## Try to read out the port. Try 5 times/sec, until timeout.
  max_i <- load_timeout / 1000 * 5
  for (i in seq_len(max_i)) {
    err_lines <- readLines(p$get_error_file())

    if (!p$is_alive()) {
      abort(paste0(
        "Error starting shiny application:\n",
        paste(err_lines, collapse = "\n")
      ), app = self)
    }
    if (any(grepl("Listening on http", err_lines))) break

    Sys.sleep(0.2)

    if (i == max_i) {
      abort(paste0(
        "Cannot find shiny port number. Error:\n",
        paste(err_lines, collapse = "\n")
      ), app = self)
    }
  }

  line <- err_lines[grepl("Listening on http", err_lines)]
  "!DEBUG shiny up and running, `line`"

  url <- sub(".*(https?://.*)", "\\1", line)
  private$shiny_url$set(url)

  invisible(self)
}
