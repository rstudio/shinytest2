
app_set_dir <- function(
  self, private,
  app_dir
) {
  ckm8_assert_app_driver(self, private)

  app_dir <- app_dir_value(app_dir)

  if (!fs::dir_exists(app_dir)) {
    cur_dir <- app_dir
    cur_dir <-
      if (is.null(cur_dir)) {
        "`NULL`"
      } else {
        paste0("\"", cur_dir, "\"")
      }
    app_abort(
      self, private,
      c(
        "`app_dir` must be an existing directory",
        "i" = paste0("Received: ", cur_dir)
      )
    )
  }

  shiny_app_and_rmd_abort <- function() {
    app_abort(self, private, c(
      "`app_dir` must be a directory containing:",
      "",
      "*" = "an `./app.R` Shiny application",
      "",
      " " = "or",
      "",
      "*" = "a Shiny R Markdown file",
      "*" = "a `./server.R` Shiny server",
      "",
      "i" = "If a Shiny R Markdown document is found, it will be the prefered document.",
      "i" = "`./app.R` is not compatible with Shiny R Markdown files."
    ))
  }
  shiny_app_and_server_abort <- function() {
    app_abort(self, private, c(
      "`app_dir` contains both `app.R` and `server.R`. Unintented behavior may occur.",
      "!" = "Please either use `app.R` or `server.R`, but not both."
    ))
  }

  if (!fs::dir_exists(app_dir)) {
    app_abort(self, private, "`app_dir` must be an existing directory")
  }

  has_app_path <- fs::file_exists(fs::path(app_dir, "app.R"))
  has_server_path <- fs::file_exists(fs::path(app_dir, "server.R"))

  rmds <- app_dir_rmd(self, private, app_dir)
  # if (length(rmds) > 1) {
  #   app_abort(self, private, "For testing, only one .Rmd file is allowed per directory.")
  # }
  if (length(rmds) > 0 && has_app_path) {
    # Has an Rmd and an app.R
    shiny_app_and_rmd_abort()
  }
  # Rmd + server.R is allowed! Use Rmd path

  # No Rmd, but has app.R or server.R
  if (has_app_path && has_server_path) {
    shiny_app_and_server_abort()
  }

  # Single app.R, server.R, or Rmd file
  private$dir <- fs::path_abs(app_dir)
}

app_get_dir <- function(self, private) {
  ckm8_assert_app_driver(self, private)
  as.character(private$dir)
}

app_dir_has_rmd <- function(self, private, app_dir = missing_arg()) {
  if (rlang::is_missing(app_dir)) {
    ckm8_assert_app_driver(self, private)
    app_dir <- self$get_dir()
  }
  length(app_dir_rmd(app_dir = app_dir)) >= 1
}
app_dir_rmd <- function(self, private, app_dir = rlang::missing_arg()) {
  if (rlang::is_missing(app_dir)) {
    ckm8_assert_app_driver(self, private)
    app_dir <- self$get_dir()
  }
  # Similar to https://github.com/rstudio/rmarkdown/issues/2236
  docs <- fs::dir_ls(app_dir, regexp = "^[^_].*\\.[Rrq][Mm][Dd]$", type = "file")

  if (length(docs) >= 1) {
    docs <- Filter(docs, f = function(doc_path) {
      front_matter <- rmarkdown::yaml_front_matter(doc_path)
      runtime <-
        # Standard rmd
        front_matter$runtime %||%
        # Quarto engine
        front_matter$server %||%
        # Invalid default value
        "static"
      # Must start with `shiny`: `shiny`, `shiny_prerendered`, `shinyrmd`
      grepl("^shiny", tolower(runtime))
    })
  }

  docs
}


app_dir_value <- function(app_dir) {
  ckm8_assert_single_string(app_dir)

  if (fs::file_exists(app_dir) && !fs::dir_exists(app_dir)) {
    # If a file was provided, use the parent directory
    app_dir <- fs::path_dir(app_dir)
  }
  app_dir
}
