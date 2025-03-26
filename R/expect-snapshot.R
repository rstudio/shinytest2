app__expect_snapshot_value <- function(
  # nolint
  self,
  private,
  x,
  ...
) {
  ckm8_assert_app_driver(self, private)
  testthat::local_edition(3)

  testthat::expect_snapshot_value(
    x,
    cran = FALSE,
    variant = self$get_variant(),
    ...
  )
}
app__expect_snapshot_file <- function(
  self,
  private,
  file,
  variant,
  name = fs::path_file(file),
  compare = testthat::compare_file_binary
) {
  ckm8_assert_app_driver(self, private)
  testthat::local_edition(3)

  # Add name prefix to saved snapshot file
  name <-
    if (is.null(private$name)) {
      name
    } else {
      paste0(private$name, "-", name)
    }
  # Make it path safe so others are not created or accessed
  name_safe <- fs::path_sanitize(name, "_")

  withCallingHandlers(
    # Display text diff when possible
    testthat::expect_snapshot_file(
      file,
      name = name_safe,
      cran = FALSE,
      compare = compare,
      variant = self$get_variant()
    ),
    expectation_failure = function(cnd) {
      if (
        # Only display text if allowed to be verbose
        is_false(getOption("shinytest2.expectation_failure.quiet", FALSE)) &&
          # Require `diffobj` to be installed
          rlang::is_installed("diffobj") &&
          # Only display text if compare fn is for text diffs
          identical(compare, testthat::compare_file_text)
      ) {
        # In vdiffr, check for snapshot version

        # Display diffs similar to vdiffr
        # https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L127-L169
        snapshotter <- get_snapshotter()
        if (!is.null(snapshotter)) {
          path_old <- snapshot_path(snapshotter, name)
          path_new <- fs::path_ext_set(
            path_old,
            paste0(".new.", fs::path_ext(path_old))
          )
          if (all(file.exists(path_old, path_new))) {
            diff <- diff_lines(path_old, path_new)
            msg <- paste0(
              "Diff in snapshot file `",
              snapshotter$file,
              name,
              "`\n",
              paste0(diff, collapse = "\n")
            )
            # Defer the expectation to be thrown after re-throwing the original condition
            # This prints the failure first, then a warning about the text diff
            withr::defer({
              testthat::exp_signal(
                testthat::new_expectation("warning", msg)
              )
            })
          }
        }
      }

      # Re-throw original condition
      stop(cnd)
    }
  )
}

get_snapshotter <- function() {
  x <- getOption("testthat.snapshotter")
  if (is.null(x)) {
    return()
  }

  if (!x$is_active()) {
    return()
  }

  x
}
snapshot_path <- function(snapshotter, file) {
  file.path(snapshotter$snap_dir, snapshotter$file, file)
}

diff_lines <- function(before_path, after_path) {
  before <- readLines(before_path)
  after <- readLines(after_path)

  diff <- diffobj::diffChr(
    before,
    after,
    format = "raw",
    # For reproducibility
    disp.width = 80
  )
  diff
}
