testthat_expect_snapshot_output <- function( # nolint
  private,
  x,
  cran = FALSE
  # , variant = NULL
) {
  testthat::expect_snapshot_output(
    x,
    cran = cran,
    variant = private$variant
  )
}
testthat_expect_snapshot_file <- function(
  private,
  file,
  name = fs::path_file(file),
  cran = FALSE,
  compare = testthat::compare_file_binary
  # , variant = NULL
) {
  # Add name prefix to saved snapshot file
  name <-
    if (is.null(private$name)) {
      name
    } else {
      paste0(private$name, "-", name)
    }

  testthat::expect_snapshot_file(
    file,
    name = name,
    cran = cran,
    compare = compare,
    variant = private$variant
  )
}


sd2_expectSnapshot <- function(
  self,
  private,
  ...,
  name = NULL,
  items = NULL,
  screenshot = NULL,
  cran = FALSE,
  error = FALSE
  # , variant = NULL
) {
  testthat::expect_s3_class(self, "ShinyDriver2")
  ellipsis::check_dots_empty()

  snapshot_info <- sd2_snapshot(self, private, items = items, name = name, screenshot = screenshot)
  # utils::str(snapshot_info)

  # compare json
  testthat_expect_snapshot_file(
    private,
    snapshot_info$json_path,
    cran = cran,
    compare = testthat::compare_file_text
    # , variant = variant
  )

  # compare screenshot
  if (!is.null(snapshot_info$screenshot_path)) {
    testthat_expect_snapshot_file(
      private,
      snapshot_info$screenshot_path,
      cran = cran,
      compare = testthat::compare_file_binary
      # , variant = variant
    )
  }

}


#' Expect a shinytest2 snapshot
#'
#'
#' @param app A [ShinyDriver2] object.
#' @param ... Must be empty. Allows for parameter expansion.
#' @param name The prefix name to be used for the snapshot. By default, this uses the name supplied to `app` on initialization.
#' @param items Elements to only be included in the snapshot. If supplied, can contain `inputs`, `output`, and `export`. Each value of `items` can either be `TRUE` (for all values) or a character list of names to use.
#' @param screenshot A boolean indicating whether to take a screenshot.
#' @inheritParams testthat::expect_snapshot_file
#' @export
expect_snapshot_app <- function(
  app,
  ...,
  name = NULL,
  items = NULL,
  screenshot = NULL,
  # variant = NULL,
  cran = FALSE
) {
  app$expectSnapshot(
    ...,
    name = name,
    items = items,
    screenshot = screenshot,
    cran = cran
    # , variant = variant
  )

}


#' @importFrom rlang !!!
sd2_expectSnapshotJS <- function(
  self,
  private,
  ...,
  script,
  arguments = list2(),
  # variant = NULL,
  post_script = NULL,
  cran = FALSE
) {
  testthat::expect_s3_class(self, "ShinyDriver2")
  ellipsis::check_dots_empty()

  result <- self$executeScript(script, !!!arguments)

  if (is.function(post_script)) {
    checkmate::assert_integer(length(formals(post_script)), lower = 1)
    result <- post_script(result)
  }

  testthat_expect_snapshot_output(
    private,
    result,
    cran = cran
    # , variant = variant
  )
}


# ShinyDriver2

#' Expect snapshot of JS script
#'
#' This is a building block function that should be called by other functions.
#' For example, [`expect_snapshot_app_dom()`] and [`expect_snapshot_app_text()`] are thin wrappers around this function.
#'
#' @param app A [ShinyDriver2] object.
#' @param script A string containing the JS script to be executed.
#' @param ... Must be empty. Allows for parameter expansion.
#' @param arguments A list of arguments to be passed to the script.
#' @param post_script A function to be called on the result of the script before taking the snapshot.
#'   [`expect_snapshot_app_dom()`] and [`expect_snapshot_app_text()`] both use [`unlist()`].
#' @inheritParams testthat::expect_snapshot_output
#' @export
expect_snapshot_app_js <- function(
  app,
  script,
  ...,
  arguments = list2(), # TODO-barret; or  make this the `...`?
  post_script = NULL,
  # variant = NULL,
  cran = FALSE
) {
  ellipsis::check_dots_empty()
  app$expectSnapshotJS(
    script = script, !!!arguments,
    # variant = variant,
    post_script = post_script,
    cran = cran
  )
}

#' Expect App DOM or text snapshot
#'
#' These methods will find all matching elements and extract their respective values.
#' These values will then be stored as a snapshot.
#' The snapshot `variant` will be extracted from the `app` object directly.
#'
#' @param app A [ShinyDriver2] object.
#' @param selector A DOM selector to be passed into jQuery
#' @param ... Must be empty. Allows for parameter expansion.
#' @inheritParams testthat::expect_snapshot_output
#' @describeIn expect_snapshot_app_dom This method will extract the text value of all matching elements via `$(el).text()`.
#'   This method is more robust to internal package change as only the text values will be return.
#'   When possible, use `expect_snapshot_app_text()` over `expect_snapshot_app_dom()` to allow package authors room to change.
#' @export
expect_snapshot_app_text <- function(
  app,
  selector,
  ...,
  # variant = NULL,
  cran = FALSE
) {
  ellipsis::check_dots_empty()
  expect_snapshot_app_js(
    app,
    script = paste0("return $(\"", selector, "\").map(function(i, item) { return $(item).text() }).get();"),
    # variant = variant,
    post_script = unlist,
    cran = cran
  )
}
#' @describeIn expect_snapshot_app_dom This method will extract the relevant DOM structure of all matching elements.
#'   This method is great for testing the full DOM structure of particular HTML elements.
#'   It is recommended to only use this method on DOM elements that you have full control over.
#'   This will help avoid false-positives when underlying packages may update.
#' @param outerHTML If `TRUE`, the full DOM structure will be returned (`el.outerHTML`).
#'   If `FALSE`, the full DOM structure of the child elements will be returned (`$(el).html()`).
#' @export
expect_snapshot_app_dom <- function(
  app,
  selector,
  ...,
  # variant = NULL,
  outerHTML = FALSE,
  cran = FALSE
) {
  ellipsis::check_dots_empty()
  html_code <-
    if (isTRUE(outerHTML)) {
      "item.outerHTML"
    } else {
      "$(item).html()"
    }
  expect_snapshot_app_js(
    app,
    script = paste0("return $(\"", selector, "\").map(function(i, item) { return ", html_code, "; }).get();"),
    # variant = variant,
    post_script = unlist,
    cran = cran
  )
}
