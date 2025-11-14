default_screenshot_args <- function(screenshot_args) {
  if (
    is_missing_value(screenshot_args) ||
      is.null(screenshot_args) ||
      isTRUE(screenshot_args)
  ) {
    screenshot_args <- list()
  }
  screenshot_args
}

app_get_screenshot <- function(
  self,
  private,
  file = NULL,
  ...,
  screenshot_args = missing_arg(),
  delay = missing_arg(),
  selector = missing_arg()
) {
  "!DEBUG app_get_screenshot()"
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  screenshot_args <- default_screenshot_args(
    maybe_missing_value(screenshot_args, private$default_screenshot_args)
  )
  if (is_false(screenshot_args)) {
    app_warn(
      self,
      private,
      "`screenshot_args` can not be `FALSE` when calling `app$get_screenshot()`. Setting to `list()`"
    )
    screenshot_args <- list()
  }
  checkmate::assert_list(screenshot_args)

  screenshot_args$delay <- maybe_missing_value(
    delay,
    screenshot_args$delay
  ) %||%
    0
  checkmate::assert_number(
    screenshot_args$delay,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )

  screenshot_args$selector <- maybe_missing_value(
    selector,
    screenshot_args$selector
  ) %||%
    "scrollable_area"
  screenshot_args <- handle_custom_selector(self, private, screenshot_args)

  if (is.null(file)) {
    self$log_message("Taking screenshot")
  } else {
    self$log_message(paste0("Taking screenshot: ", file))
  }
  path <- st2_temp_file(".png")
  screenshot_args$filename <- path

  do.call(self$get_chromote_session()$screenshot, screenshot_args)

  # Fix up the PNG resolution header on windows
  if (is_windows()) {
    normalize_png_res_header(self, private, path)
  }

  if (is.null(file)) {
    # Display file in graphics device
    rlang::check_installed("showimage")
    withr::with_par(list(bg = "grey90"), {
      showimage::show_image(path)
    })
  } else {
    fs::file_copy(path, file)
  }

  invisible(self)
}


app_expect_screenshot <- function(
  self,
  private,
  ...,
  threshold = getOption("shinytest2.compare_screenshot.threshold", NULL),
  kernel_size = getOption("shinytest2.compare_screenshot.kernel_size", 5),
  quiet = FALSE,
  name = NULL,
  screenshot_args = missing_arg(),
  delay = missing_arg(),
  selector = missing_arg(),
  compare = missing_arg()
) {
  "!DEBUG app_expect_screenshot()"
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  compare <- rlang::maybe_missing(
    compare,
    function(old, new) {
      compare_screenshot_threshold(
        old,
        new,
        threshold = threshold,
        kernel_size = kernel_size,
        quiet = quiet
      )
    }
  )

  filename <- app_next_temp_snapshot_path(self, private, name, "png")

  # Announce snapshot file before touching before any other expressions can fail
  testthat::announce_snapshot_file(filename)

  # Take screenshot
  self$get_screenshot(
    file = filename,
    screenshot_args = screenshot_args,
    delay = delay,
    selector = selector
  )

  # Assert screenshot value
  app__expect_snapshot_file(
    self,
    private,
    filename,
    compare = compare
  )
}

app_expect_screenshot_and_variant <- # nolint
  function(
    self,
    private,
    ...
  ) {
    if (app_is_missing_variant(self, private)) {
      app_abort(
        self,
        private,
        c(
          "This `AppDriver` object can not call `$expect_screenshot()` without a `variant` initialized. Please supply a `variant` value when creating your `AppDriver` object, like `AppDriver(variant = <value>)`",
          i = "`variant = platform_variant()` is the suggested value",
          i = "`variant = NULL` can work, but screenshots are known to cause conflicts when changing R version or platform."
        )
      )
    }
    # Expect screenshot
    app_expect_screenshot(self, private, ...)
  }


handle_custom_selector <- function(self, private, screenshot_args) {
  ckm8_assert_app_driver(self, private)

  selector <- screenshot_args$selector

  # Quit early
  if (length(selector) != 1 || !is.character(selector)) {
    return(screenshot_args)
  }
  # If not an option, quit early
  if (
    !(selector == "viewport" ||
      selector == "scrollable_area")
  ) {
    return(screenshot_args)
  }

  ## Make a cliprect that is the maximum of the window size and the root node size
  # Resolved: https://github.com/rstudio/shinytest2/issues/293
  ## https://github.com/rstudio/chromote/blob/e1d2997932671642d12bef0b4c58611e322035c7/R/screenshot.R#L28
  # > a numeric vector with 4 elements (for left, top, width, and height)
  # Related: set `html` height to viewport size https://github.com/rstudio/shinycoreci/blob/de7676a9eca61e4dd191c0eb2e6f5c65b9119c8a/inst/apps/303-bslib-html-template/template.html#L9
  screenshot_args$cliprect <-
    switch(
      selector,
      "viewport" = {
        # message("Using viewport height and width!")
        # Get the window height and width
        window_size <- self$get_window_size()
        scroll_start <- self$get_js(
          "(function() {
          return {
            scrollX: window.scrollX,
            scrollY: window.scrollY
          }
        })()"
        )
        # > a numeric vector with 4 elements (for left, top, width, and height)
        c(
          scroll_start$scrollX,
          scroll_start$scrollY,
          window_size$width,
          window_size$height
        )
      },
      "scrollable_area" = {
        # message("Using scroll height and width!")
        scroll_size <- self$get_js(
          "(function() {
          const html = document.querySelector('html')
          return {
            height: html.scrollHeight,
            width: html.scrollWidth
          }
        })()"
        )
        c(0, 0, scroll_size$width, scroll_size$height)
      },
      # Safety
      stop("Can not create cliprect for unknown selector: ", selector)
    )

  return(screenshot_args)
}
