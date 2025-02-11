# Do not delete
# Paired with tests/testthat/apps/test-env/ 's tests on `.internal_value_used_by_shinytest2_test`
.internal_value_used_by_shinytest2_test <- TRUE

app_start_shiny <- function(
  self,
  private,
  ...,
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

  try_null <- function(expr) {
    tryCatch(expr, error = function(e) {
      NULL
    })
  }

  package_path <- try_null(pkgload::pkg_path())
  package_name <- try_null(pkgload::pkg_name())

  # `testthat::is_checking()` is TRUE inside `testthat::test_check()`, typically called in `R CMD check`.
  # If we're doing R CMD check, we only want to use installed packages.
  load_package <- if (testthat::is_checking()) "installed" else "source"

  p <- local({
    # https://github.com/r-lib/testthat/issues/603
    withr::local_envvar(c("R_TESTS" = NA))
    # Load the app's .Rprofile / .Renviron if possible
    withr::local_dir(self$get_dir())

    callr::r_bg(
      stdout = sprintf(tempfile_format, "shiny-stdout"),
      stderr = sprintf(tempfile_format, "shiny-stderr"),
      supervise = TRUE,
      args = list(
        .app_dir = self$get_dir(),
        .shiny_args = shiny_args,
        .has_rmd = app_dir_has_rmd(self, private),
        .seed = seed,
        .rng_kind = rng_kind,
        .render_args = render_args,
        .options = options,
        .package_name = package_name,
        .package_path = package_path,
        .load_package = load_package
      ),
      function(
        .app_dir,
        .shiny_args,
        .has_rmd,
        .seed,
        .rng_kind,
        .render_args,
        .options,
        .package_name,
        .package_path,
        .load_package
      ) {
        if (!is.null(.seed)) {
          # Prior to R 3.6, RNGkind has 2 args, otherwise it has 3
          do.call(RNGkind, as.list(.rng_kind))
          set.seed(.seed)
          getNamespace("shiny")$withPrivateSeed(set.seed(.seed + 11))
        }

        .options <- as.list(.options)
        .options[["shiny.testmode"]] <- TRUE
        # TODO-future; Adjust shiny to add htmldeps to the list of the rendered page
        # options[["shiny-testmode-html-dep"]] <- getTracerDep()
        do.call(base::options, .options)

        # Taken inspiration from `testthat:::test_files_setup` but trying to
        # never library the package or {testthat}. Instead attach the local
        # package (when possible).
        # Motivation:
        # * Shiny will only have access to the installed package namepace after
        #   a library call, so during testing we should try to mimic this
        #   behavior to avoid surprises
        # * Whereas testthat wants to have already `library()`ed {testthat} and
        #   the package by the time the test file is sourced

        if (!is.null(.package_path)) {
          # If .load_package == "installed", carry on like normal!

          if (.load_package == "source") {
            # Shim the local package. Only expose like a regular package.
            # Reasoning is that Shiny will only have access to the installed
            # package when not using the local package.

            pkgload::load_all(
              .package_path,
              # Be sure to attach the local package to the search path
              attach = TRUE,
              # Expose only exported values! No imports or helpers
              export_all = FALSE,
              export_imports = FALSE,
              helpers = FALSE,
              quiet = TRUE
            )
          }
        }

        ret <-
          if (.has_rmd) {
            # Shiny document
            rmarkdown::run(
              file = NULL, # Do not open anything in browser
              dir = .app_dir,
              default_file = NULL, # Let rmarkdown find the default file
              # DO NOT ENABLE! Makes things like `app$wait_for_idle()` not work as expected.
              auto_reload = FALSE, # Do not constantly poll for file changes. Drastically reduces `app$get_logs()`
              shiny_args = .shiny_args,
              render_args = .render_args
            )
          } else {
            # Normal shiny app
            do.call(shiny::runApp, c(.app_dir, .shiny_args))
          }

        # Return value is important for `AppDriver$stop()`
        return(ret)
      }
    )
  })

  private$shiny_process <- p # nolint

  "!DEBUG waiting for shiny to start"
  if (!p$is_alive()) {
    app_abort(
      self,
      private,
      paste0(
        "Failed to start shiny. Error:\n",
        strwrap(readLines(p$get_error_file()))
      )
    )
  }

  "!DEBUG finding shiny port"
  ## Try to read out the port. Try 5 times/sec, until timeout.
  max_i <- floor(load_timeout / 1000 * 5)
  err_lines <- ""
  for (i in seq_len(max_i)) {
    err_lines <- readLines(p$get_error_file())

    if (!p$is_alive()) {
      app_abort(
        self,
        private,
        paste0(
          "Error starting shiny application:\n",
          paste(err_lines, collapse = "\n")
        )
      )
    }
    if (any(grepl("Listening on http", err_lines, fixed = TRUE))) break

    Sys.sleep(0.2)

    if (i == max_i) {
      app_abort(
        self,
        private,
        sprintf(
          paste(
            "The Shiny app failed to start up within %s second%s.",
            "To increase the loading timeout, consult the documentation in `?AppDriver`",
            "for the `load_timeout` argument of `AppDriver$new()`.",
            "The app printed the following lines to stderr during start up:\n%s"
          ),
          load_timeout / 1000,
          if (load_timeout == 1000) "" else "s",
          paste(err_lines, collapse = "\n")
        )
      )
    }
  }

  line <- err_lines[grepl("Listening on http", err_lines, fixed = TRUE)]
  "!DEBUG shiny up and running, `line`"

  url <- sub(".*(https?://.*)", "\\1", line)
  private$shiny_url$set(url)

  invisible(self)
}
