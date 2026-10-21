# https://github.com/rstudio/shinytest2/issues/448
test_that("app_wait_for_serving() returns FALSE for an unbound port", {
  # A random "unused" port can be taken by another process on busy CI machines,
  # so pretend the port is never up
  local_mocked_bindings(is_up = function(...) FALSE, .package = "pingr")

  # Nothing is listening, so the wait should give up and report the port never
  # came up (rather than blocking or erroring).
  start <- Sys.time()
  expect_false(app_wait_for_serving("http://127.0.0.1:1234/", timeout = 200))
  # Should honor the timeout rather than hang indefinitely.
  expect_lt(as.numeric(Sys.time() - start, units = "secs"), 5)
})

test_that("app_wait_for_serving() returns TRUE once the port accepts connections", {
  skip_if_not_installed("httpuv")

  port <- httpuv::randomPort()
  server <- httpuv::startServer("127.0.0.1", port, list())
  withr::defer(server$stop())

  url <- sprintf("http://127.0.0.1:%s/", port)
  expect_true(app_wait_for_serving(url, timeout = 5000))
})
