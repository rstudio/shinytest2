
app_start_shiny <- function(
  self, private,
  seed = NULL,
  load_timeout = 10000,
  shiny_args = list(),
  render_args = NULL,
  options = list()
) {
  ckm8_assert_app_driver(self, private)

  tempfile_format <- st2_temp_file(pattern = "%s-", fileext = ".log")

  # the RNG kind should inherit from the parent process
  rng_kind <- RNGkind()

  p <- withr::with_envvar(
    c("R_TESTS" = NA),
    callr::r_bg(
      function(app_dir, shiny_args, has_rmd, seed, rng_kind, render_args, options) {

        if (!is.null(seed)) {
          # Prior to R 3.6, RNGkind has 2 args, otherwise it has 3
          do.call(RNGkind, as.list(rng_kind))
          set.seed(seed)
          getNamespace("shiny")$withPrivateSeed(set.seed(seed + 11))
        }

        options <- as.list(options)
        options[["shiny.testmode"]] <- TRUE
        # TODO-future; Adjust shiny to add htmldeps to the list of the rendered page
        # options[["shiny-testmode-html-dep"]] <- getTracerDep()
        do.call(base::options, options)

        # Return value is important for `AppDriver$stop()`
        # Do not add code after this if else block
        if (has_rmd) {
          # Shiny document
          rmarkdown::run(
            file = NULL, # Do not open anything in browser
            dir = app_dir,
            default_file = NULL, # Let rmarkdown find the default file
            # DO NOT ENABLE! Makes things like `app$wait_for_idle()` not work as expected.
            auto_reload = FALSE, # Do not constantly poll for file changes. Drastically reduces `app$get_logs()`
            shiny_args = shiny_args,
            render_args = render_args
          )
        } else {
          # Normal shiny app
          do.call(shiny::runApp, c(app_dir, shiny_args))
        }
      },
      args = list(
        app_dir = self$get_dir(),
        shiny_args = shiny_args,
        has_rmd = app_dir_has_rmd(self, private),
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
    app_abort(self, private, paste0(
      "Failed to start shiny. Error:\n",
      strwrap(readLines(p$get_error_file()))
    ))
  }

  "!DEBUG finding shiny port"
  ## Try to read out the port. Try 5 times/sec, until timeout.
  max_i <- floor(load_timeout / 1000 * 5)
  for (i in seq_len(max_i)) {
    err_lines <- readLines(p$get_error_file())

    if (!p$is_alive()) {
      app_abort(self, private, paste0(
        "Error starting shiny application:\n",
        paste(err_lines, collapse = "\n")
      ))
    }
    if (any(grepl("Listening on http", err_lines, fixed = TRUE))) break

    Sys.sleep(0.2)

    if (i == max_i) {
      app_abort(self, private, paste0(
        "Cannot find shiny port number. Error lines found:\n",
        paste(err_lines, collapse = "\n")
      ))
    }
  }

  line <- err_lines[grepl("Listening on http", err_lines, fixed = TRUE)]
  "!DEBUG shiny up and running, `line`"

  url <- sub(".*(https?://.*)", "\\1", line)
  private$shiny_url$set(url)

  invisible(self)
}
