test_that("Url Class behaves as expected", {
  expect_url <- function(url, expected_url) {
    expect_equal(Url$new()$set(url)$get(), expected_url)
  }

  expect_url("http://a.b.com", "http://a.b.com/")
  expect_url("https://a.b.com/", "https://a.b.com/")
  expect_url("http://a.b.com:1020", "http://a.b.com:1020/")
  expect_url("http://a.b.com:1020/", "http://a.b.com:1020/")
  expect_url("http://a.b.com:1020/abc", "http://a.b.com:1020/abc")
  expect_url("http://a.b.com:1020/abc/", "http://a.b.com:1020/abc/")
  expect_url("http://a.b.com/abc/", "http://a.b.com/abc/")
  expect_url("http://user@a.b.com/", "http://user@a.b.com/")
  expect_url("http://user:pass@a.b.com/", "http://user:pass@a.b.com/")

  expect_url("http:/a.b.com/", "http://a.b.com/")

  # Malformed URLs, or non-http/https protocol
  expect_url_error <- function(url, ...) {
    expect_error(Url$new()$set(url), ...)
  }
  expect_warning(
    expect_url_error("http://a.b.com:12ab/", "url port")
  )
  expect_url_error("ftp://a.b.com/", "url scheme")
})


# https://github.com/rstudio/shinytest2/issues/357
test_that("Url$combine constructs correct URL without query params", {
  url <- Url$new()
  url$set("http://127.0.0.1:4895/")
  result <- url$combine("session/abc123/download/downloadData?w=")
  expect_equal(
    result,
    "http://127.0.0.1:4895/session/abc123/download/downloadData?w="
  )
})

test_that("Url$combine preserves base query params", {
  url <- Url$new()
  url$set("http://127.0.0.1:4895/?foo=bar")
  result <- url$combine("session/abc123/download/downloadData?w=")
  parsed <- httr2::url_parse(result)
  # Path should not contain query string fragments
  expect_equal(parsed$path, "/session/abc123/download/downloadData")
  # Both the sub_url query param and the base query param should be present
  expect_true("w" %in% names(parsed$query))
  expect_equal(parsed$query$foo, "bar")
})

test_that("Url$combine handles multiple base query params", {
  url <- Url$new()
  url$set("http://127.0.0.1:4895/?foo=bar&baz=qux")
  result <- url$combine("session/abc123/download/downloadData?w=")
  parsed <- httr2::url_parse(result)
  expect_equal(parsed$query$foo, "bar")
  expect_equal(parsed$query$baz, "qux")
  expect_true("w" %in% names(parsed$query))
})

test_that("Url$combine normalizes path boundary without trailing slash", {
  url <- Url$new()
  url$set("http://127.0.0.1:4895/app")
  result <- url$combine("session/abc123/download/downloadData?w=")
  parsed <- httr2::url_parse(result)
  expect_equal(parsed$path, "/app/session/abc123/download/downloadData")
})

test_that("Url$combine normalizes path boundary with double slash", {
  url <- Url$new()
  url$set("http://127.0.0.1:4895/app/")
  result <- url$combine("/session/abc123/download/downloadData?w=")
  parsed <- httr2::url_parse(result)
  expect_equal(parsed$path, "/app/session/abc123/download/downloadData")
})
