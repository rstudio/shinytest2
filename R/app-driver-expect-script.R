app_expect_script <- function(
  self, private,
  script,
  arguments = list(),
  ...,
  timeout = 15 * 1000,
  post_fn = unlist,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()
  arguments <- as.list(arguments)

  result <- self$execute_script(
    script = script,
    arguments = arguments,
    timeout = timeout
  )

  if (is.function(post_fn)) {
    checkmate::assert_integer(length(formals(post_fn)), lower = 1)
    result <- post_fn(result)
  }

  # Must use _value_ output as _print_ output is unstable
  # over different R versions and locales
  testthat_expect_snapshot_value(
    private,
    result,
    cran = cran
  )
}


app_expect_text <- function(
  self, private,
  selector,
  ...,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  self$expect_script(
    script = paste0("return Array.from(document.querySelectorAll(\"", selector, "\")).map(function(item, i) { return item.textContent; });"),
    post_fn = unlist,
    cran = cran
  )

  invisible(self)
}


app_expect_html <- function(
  self, private,
  selector,
  ...,
  outer_html = FALSE,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  html_code <-
    if (isTRUE(outer_html)) {
      "item.outerHTML"
    } else {
      "item.innerHTML"
    }

  self$expect_script(
    script = paste0("return Array.from(document.querySelectorAll(\"", selector, "\")).map(function(item, i) { return ", html_code, "; });"),
    post_fn = unlist,
    cran = cran
  )

  invisible(self)
}
