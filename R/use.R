#' Use \pkg{shinytest2} with your Shiny application
#'
#' @describeIn use_shinytest2
#' This \pkg{usethis}-style method initializes many different useful features when using
#' \pkg{shinytest2}:
#' * `runner`: Creates a \pkg{shinytest2} test runner at `./tests/testthat.R`. This file
#' will contain a call to [`test_app()`].
#' * `setup`: Creates `./tests/testthat/setup-shinytest2.R` to add your Shiny `./R` objects and functions into the testing environment. This file will run before testing begins.
#' * `ignore`: Add an entry to `./Rbuildignore` (if it exists) and `.gitignore` to ignore new debug screenshots. (`*_.new.png`)
#' * `package`: Adds `shinytest` to the `Suggests` packages in the `DESCRIPTION` file (if it exists).
#'
#' If any of these values are _not_ missing, the remaining missing values will be set to `FALSE`. This allows `use_shinytest2()` to add more flags in future versions without opting into all changes inadvertently.
#'
#' @param app_dir The base directory for the Shiny application
#' @param runner If `TRUE`, creates a \pkg{shinytest2} test runner at `./tests/testthat.R`
#' @param setup If `TRUE`, creates a setup file called
#' `./tests/testthat/setup-shinytest2.R` containing a call to [`load_app_env()`]
#' @param ignore If `TRUE`, adds entries to `.Rbuildignore` and `.gitignore` to
#' ignore new debug screenshots. (`*_.new.png`)
#' @param package If `TRUE`, adds \pkg{shinytest2} to `Suggests` in the `DESCRIPTION` file.
#' @param ... Must be empty. Allows for parameter expansion.
#' @param quiet If `TRUE`, console output will be suppressed.
#' @param overwrite If `TRUE`, the test file or test runner will be overwritten.
#' @export
#' @examples
#' # Set up shinytest2 testing configs
#' \dontrun{use_shinytest2()}
use_shinytest2 <- function(
  app_dir = ".",
  runner = missing_arg(),
  setup = missing_arg(),
  ignore = missing_arg(),
  package = missing_arg(),
  ...,
  quiet = FALSE,
  overwrite = FALSE
) {
  rlang::check_dots_empty()

  if (all(
    rlang::is_missing(runner),
    rlang::is_missing(setup),
    rlang::is_missing(ignore),
    rlang::is_missing(package)
  )) {
    runner <- TRUE
    setup <- TRUE
    ignore <- TRUE
    package <- TRUE
  } else {
    # If something is provided, disable everything else
    runner <- isTRUE(rlang::maybe_missing(runner, FALSE))
    setup <- isTRUE(rlang::maybe_missing(setup, FALSE))
    ignore <- isTRUE(rlang::maybe_missing(ignore, FALSE))
    package <- isTRUE(rlang::maybe_missing(package, FALSE))
  }

  if (all(!runner, !setup, !ignore, !package)) {
    stop("At least one of `runner`, `setup`, `ignore`, or `package` must be `TRUE`")
  }

  if (runner)  use_shinytest2_runner(app_dir, quiet = quiet, overwrite = overwrite)
  if (setup)   use_shinytest2_setup(app_dir, quiet = quiet)
  if (ignore)  use_shinytest2_ignore(app_dir, quiet = quiet)
  if (package) use_shinytest2_package(app_dir, quiet = quiet)

  invisible()
}

#' @describeIn use_shinytest2
#' Creates a test file called `./tests/testthat/test-shinytest2.R`. By
#' default, this file's template test will initialize your Shiny application and
#' expect the initial values.
#'
#' This method will also set up a test runner if it does not exist.
#' @param open If `TRUE`, the test file will be opened in an editor via
#' [`file.edit()`] after saving.
#' @export
#' @examples
#' # Set up a shinytest2 test
#' \dontrun{use_shinytest2_test()}
use_shinytest2_test <- function(
  app_dir = ".",
  open = rlang::is_interactive(),
  quiet = FALSE,
  overwrite = FALSE
) {
  # Make sure runner exists!
  # Wrap in if statement to allow for printing if it does not exist and not printing if it does
  if (!fs::dir_exists(fs::path(app_dir, "tests/testthat.R"))) {
    use_shinytest2_runner(app_dir, quiet = quiet, overwrite = FALSE)
  }

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

use_shinytest2_setup <- function(app_dir = ".", quiet = FALSE) {
  withr::with_dir(app_dir, {
    # Legacy support for old setup.R files.
    # Should be using `setup-shinytest2.R`
    if (has_load_app_env("tests/testthat/setup.R")) {
      return(FALSE)
    }

    fs::dir_create("tests/testthat")
    write_union(
      "tests/testthat/setup-shinytest2.R",
      comments = "# Load application support files into testing environment",
      lines = "shinytest2::load_app_env()",
      quiet = quiet
    )
  })
}


use_shinytest2_package <- function(app_dir = ".", quiet = FALSE) {
  app_dir <- app_dir_value(app_dir)
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

    rlang::check_installed("usethis")
    with_this_project({
      wrapper <-
        if (quiet) {
          function(...) {
            utils::capture.output(..., type = "message")
          }
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

  # Do not use `usethis::use_git_ignore()` or `usethis::use_build_ignore()` directly!
  # The functions have sticky paths once set. Instead, use their inner logic via
  # `usethis::write_union(FILE, LINES, quiet = quiet)`
  app_dir <- app_dir_value(app_dir)
  withr::with_dir(app_dir, {
    wrote_lines <- write_union(
      ".gitignore",
      comments = c("# {shinytest2}: Ignore new debug snapshots for `$expect_values()`"),
      lines = "*_.new.png",
      quiet = quiet
    )
    if (!quiet) {
      if (wrote_lines) {
        ## `write_union()` is verbose, do not be double verbose
        # rlang::inform(c("*" = "Added `*_.new.png` to `", fs::path(app_dir, ".gitignore"), "`"))
      } else {
        rlang::inform(
          c(
            "!" = paste0("`", fs::path(app_dir, ".gitignore"), "` already contains `*_.new.png`")
          )
        )
      }
    }

    if (fs::file_exists(fs::path(app_dir, ".Rbuildignore"))) {
      build_ignores <- c(
        "_\\.new\\.png$"
      )
      wrote_lines <- write_union(".Rbuildignore", lines = build_ignores, quiet = quiet)
      if (!quiet) {
        if (wrote_lines) {
          ## `write_union()` is verbose, do not be double verbose
          # rlang::inform(c("*" = "Added `_*.new.png` to `", fs::path(app_dir, ".Rbuildignore"), "`"))
        } else {
          rlang::inform(
            c(
              "!" = paste0("`", fs::path(app_dir, ".Rbuildignore"), "` already contains `_*.new.png`")
            )
          )
        }
      }
    } else {
      if (!quiet) {
        rlang::inform(c("!" = "No `.Rbuildignore` file found. Skipping adding `_*.new.png` to `.Rbuildignore`"))
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

  app_dir <- app_dir_value(app_dir)
  withr::with_dir(app_dir, {

    if (!overwrite && fs::file_exists(to_file)) {
      if (!quiet) {
        if (identical(readLines(from_file), readLines(to_file))) {
          # Notify that the file is identical
          rlang::inform(c("*" = paste0("Identical file found. Skipping: ", to_file)))
        } else {
          # Notify that a file already exists
          rlang::inform(c("!" = paste0(existing_pre_msg, to_file)))
        }
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

    edit_file(to_file, open = open)
    TRUE
  })
}


edit_file <- function(file, open = TRUE) {
  if (isTRUE(open)) {
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
