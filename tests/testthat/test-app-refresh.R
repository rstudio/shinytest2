

test_that("app refresh works", {
  skip("refresh does not work")

  app <- ShinyDriver2$new(test_path("apps/refresh"))
  app$waitForValue("time", iotype = "output")

  before_time <- as.numeric(app$getValue("time"))
  Sys.sleep(2)
  app$refresh()
  app$waitForValue("time", iotype = "output")
  after_time <- as.numeric(app$getValue("time"))

  expect_failure(
    # expect_equal(1634653779, 1634653780)
    expect_equal(before_time, after_time)
  )

})
