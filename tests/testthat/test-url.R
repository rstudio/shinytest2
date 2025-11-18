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
