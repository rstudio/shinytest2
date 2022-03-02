# TODO-barret; Auto accept all new debug snapshots? This will remove the requirement to ignore _.new.png

#' Use \pkg{shinytest2} methods
#'
#' This unified method initializes many different useful features when using
#' \pkg{shinytest2}. See `actions` below for more details.
#'
#' @param app_dir The base directory for the Shiny application
#' @param ... Must be empty. Allows for parameter expansion.
#' @param open If `TRUE`, the test file will be opened in an editor via
#' [`file.edit()`] after saving.
#' @param quiet If `TRUE`, console output will be suppressed.
#' @param overwrite If `TRUE`, the test file or test runner will be overwritten.
#' @param actions Actions that can be performed:
#'  * `"runner"`: Create a \pkg{shinytest2} test runner at `./tests/testthat.R`
#'  * `"test_file"`: Create a \pkg{shinytest2} test file at `./tests/testthat/test-shinytest2.R`
#'  * `"ignore"`: Add entries to `.Rbuildignore` and `.gitignore` to ignore new debug screenshots. (`*_.new.png`)
#'  * `"package"`: Add \pkg{shinytest2} to `Suggests` in the `DESCRIPTION` file.
#' @export
#' @examples
#' \dontrun{use_shinytest2()}
use_shinytest2 <- function(
  app_dir = ".",
  ...,
  open = rlang::is_interactive(),
  quiet = FALSE,
  overwrite = FALSE,
  actions = c("runner", "test_file", "ignore", "package")
) {
  ellipsis::check_dots_empty()

  actions <- match.arg(actions, several.ok = TRUE)
  for (action in actions) {
    switch(action,
      package = use_shinytest2_package(app_dir, quiet = quiet),
      ignore = use_shinytest2_ignore(app_dir, quiet = quiet),
      runner = use_shinytest2_runner(app_dir, quiet = quiet, overwrite = overwrite),
      test_file = use_shinytest2_test_file(app_dir, quiet = quiet, overwrite = overwrite, open = open),
      stop(paste0("Unknown action: ", action))
    )
  }
  invisible()
}

use_shinytest2_package <- function(app_dir = ".", quiet = FALSE) {
  rlang::check_installed("usethis")
  withr::with_dir(app_dir, {
    if (!fs::file_exists("DESCRIPTION")) {
      if (!quiet) {
        rlang::inform(
          c("!" = paste0(
            "No `", fs::path(app_dir, "DESCRIPTION"), "` file found.",
            " Skipping adding `{shinytest2}` to `Suggests`"
          ))
        )
      }
      return(FALSE)
    }
    ## No need for comments, usethis::use_package() provides messages
    # if (!quiet) rlang::inform(c("*" = "Adding `shinytest2` to `Suggests` in `DESCRIPTION` file"))

    with_this_project({
      wrapper <-
        if (quiet) function(...) {
          capture.output(..., type = "message")
        } else {
          force
        }
      wrapper({
        usethis::use_package("shinytest2", "Suggests")
      })
    })
  })
  TRUE
}

use_shinytest2_ignore <- function(app_dir = ".", quiet = FALSE) {

  # Check app_dir location?
  rlang::check_installed("usethis")

  # Do not use `usethis::use_git_ignore()` or `usethis::use_build_ignore()` directly!
  # The functions have sticky paths once set. Instead, use their inner logic via
  # `usethis::write_union(FILE, LINES)`
  withr::with_dir(app_dir, {
    git_ignores <- c(
      "# {shinytest2}: Ignore new debug snapshots for `$expect_values()`",
      "*_.new.png"
    )
    # TODO-barret; write-union is verbose, do not be double verbose
    wrote_lines <- usethis::write_union(".gitignore", git_ignores)
    if (!quiet) {
      if (wrote_lines) {
        rlang::inform(c("*" = "Added `*_.new.png` to `", fs::path(app_dir, ".gitignore"), "`"))
      } else {
        rlang::inform(c("!" = "`", fs::path(app_dir, ".gitignore"), "` already contains `*_.new.png`"))
      }
    }

    build_ignores <- c(
      "_\\.new\\.png$"
    )
    wrote_lines <- usethis::write_union(".Rbuildignore", build_ignores)
    if (!quiet) {
      if (wrote_lines) {
        rlang::inform(c("*" = "Added `_*.new.png` to `", fs::path(app_dir, ".Rbuildignore"), "`"))
      } else {
        rlang::inform(c("!" = "`", fs::path(app_dir, ".Rbuildignore"), "` already contains `_*.new.png`"))
      }
    }
  })
}

use_shinytest2_runner <- function(app_dir = ".", quiet = FALSE, overwrite = FALSE) {
  copy_test_file_helper(
    from_file = system.file("internal/template/testthat.R", package = "shinytest2"),
    to_file = "tests/testthat.R",
    pre_msg = "Saving test runner: ",
    existing_pre_msg = "Runner already found: ",
    app_dir = app_dir,
    quiet = quiet,
    overwrite = overwrite,
    open = FALSE
  )
}

use_shinytest2_test_file <- function(app_dir = ".", open = rlang::is_interactive(), quiet = FALSE, overwrite = FALSE) {
  copy_test_file_helper(
    from_file = system.file("internal/template/test-shinytest2.R", package = "shinytest2"),
    to_file = "tests/testthat/test-shinytest2.R",
    pre_msg = "Saving test: ",
    existing_pre_msg = "Test already found: ",
    app_dir = app_dir,
    quiet = quiet,
    overwrite = overwrite,
    open = open
  )
}

copy_test_file_helper <- function(
  from_file,
  to_file,
  pre_msg,
  existing_pre_msg,
  app_dir = ".",
  quiet = FALSE,
  overwrite = FALSE,
  open = FALSE
) {

  withr::with_dir(app_dir, {

    if (!overwrite && fs::file_exists(to_file)) {
      if (!quiet) {
        rlang::inform(c("!" = paste0(existing_pre_msg, to_file)))
      }
      return(FALSE)
    }
    fs::dir_create(fs::path_dir(to_file))
    if (!quiet) {
      rlang::inform(c("*" = paste0(pre_msg, to_file)))
    }
    fs::file_copy(
      from_file,
      to_file,
      overwrite = TRUE
    )

    if (open) edit_file(to_file)
    TRUE
  })
}


edit_file <- function(file) {
  if (isTRUE(file)) {
    if (rlang::is_installed("usethis")) {
      usethis::edit_file(file)
    } else {
      utils::file.edit(file)
    }
  }
}

# Use `force = TRUE` to set up infrastructure without forcing to look for a DESCRIPTION file
with_this_project <- function(code, ..., path = ".", force = TRUE) {
  rlang::check_installed("usethis")
  usethis::with_project(path = path, code = code, force = force, ..., quiet = FALSE)
}
