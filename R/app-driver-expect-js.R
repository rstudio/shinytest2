app_js_script_helper <- function(self, private, script = missing_arg(), file = missing_arg()) {
  ckm8_assert_app_driver(self, private)
  if (rlang::is_missing(file)) return(script)

  if (!rlang::is_missing(script)) {
    app_warn(self, private,
      "Both `file` and `script` are specified. `script` will be ignored."
    )
  }
  read_utf8(file)
}

app_get_js <- function(
  self, private,
  script,
  ...,
  file = missing_arg(),
  timeout = missing_arg()
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()
  timeout <- app_get_timeout(self, private, timeout = timeout)

  "!DEBUG app_get_js()"
  chromote_eval(
    self$get_chromote_session(),
    app_js_script_helper(self, private, script, file),
    timeout = timeout,
    awaitPromise = TRUE,
    returnByValue = TRUE
  )$result$value
}
app_run_js <- function(
  self, private,
  script,
  ...,
  file = missing_arg(),
  timeout = missing_arg()
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()
  timeout <- app_get_timeout(self, private, timeout = timeout)

  "!DEBUG app_run_js()"
  chromote_eval(
    self$get_chromote_session(),
    app_js_script_helper(self, private, script, file),
    timeout = timeout,
    awaitPromise = FALSE,
    returnByValue = FALSE,
    allow_no_response = TRUE # enables `awaitPromise` and `returnByValue` to be FALSE
  )

  invisible(self)
}


app_expect_js <- function(
  self, private,
  script,
  ...,
  file = missing_arg(),
  timeout = missing_arg(),
  pre_snapshot = NULL,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()
  timeout <- app_get_timeout(self, private, timeout = timeout)

  result <- self$get_js(
    script = script,
    file = file,
    timeout = timeout
  )

  if (is.function(pre_snapshot)) {
    checkmate::assert_integer(length(formals(pre_snapshot)), lower = 1)
    result <- pre_snapshot(result)
  }

  # Must use _value_ output as _print_ output is unstable
  # over different R versions and locales
  app__expect_snapshot_value(
    self, private,
    result,
    cran = cran
  )
}


get_text_js <- function(selector) {
  paste0(
    "Array.from(document.querySelectorAll(", toJSON_atomic(selector), ")).map((item, i) => item.textContent);"
  )
}
app_get_text <- function(
  self, private,
  selector
) {
  ckm8_assert_app_driver(self, private)
  # ellipsis::check_dots_empty()

  ret <- self$get_js(
    script = get_text_js(selector)
  )
  unlist(ret)
}
app_expect_text <- function(
  self, private,
  selector,
  ...,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  self$expect_js(
    script = get_text_js(selector),
    pre_snapshot = unlist,
    cran = cran
  )

  invisible(self)
}


get_html_js <- function(selector, outer_html) {
  paste0(
    "let map_fn = ", toJSON_atomic(outer_html), " ? (item, i) => item.outerHTML : (item, i) => item.innerHTML;\n",
    "Array.from(document.querySelectorAll(", toJSON_atomic(selector), ")).map(map_fn);"
  )
}
app_get_html <- function(
  self, private,
  selector,
  ...,
  outer_html = TRUE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  ret <- self$get_js(
    script = get_html_js(selector, isTRUE(outer_html))
  )
  unlist(ret)
}
app_expect_html <- function(
  self, private,
  selector,
  ...,
  outer_html = TRUE,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  self$expect_js(
    script = get_html_js(selector, isTRUE(outer_html)),
    pre_snapshot = unlist,
    cran = cran
  )

  invisible(self)
}
