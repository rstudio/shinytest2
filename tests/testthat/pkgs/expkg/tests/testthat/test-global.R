library(shinytest2)

test_that("App object does not see test objects in .GlobalEnv", {
  assign("expkg_data", "from the test", envir = globalenv())
  withr::defer(rm("expkg_data", envir = globalenv()))

  app <- AppDriver$new(expkg_global_app())
  expect_equal(app$get_value(output = "data"), "<missing>")
})

test_that("Closure copies test objects into the background .GlobalEnv", {
  expkg_data <- "from the test"

  app <- AppDriver$new(function() {
    assign("expkg_data", expkg_data, envir = globalenv())
    library(expkg)
    expkg_global_app()
  })
  expect_equal(app$get_value(output = "data"), "from the test")
})

test_that("Closure can pass test objects through an env argument", {
  expkg_data <- "from the test"

  app <- AppDriver$new(function() {
    library(expkg)
    expkg_global_app(env = list2env(list(expkg_data = expkg_data)))
  })
  expect_equal(app$get_value(output = "data"), "from the test")
})

test_that("Closure can run an app dir with test objects", {
  expkg_data <- "from the test"
  app_dir <- normalizePath(test_path("apps/global"))

  app <- AppDriver$new(function() {
    assign("expkg_data", expkg_data, envir = globalenv())
    shiny::runApp(app_dir)
  })
  expect_equal(app$get_value(output = "data"), "from the test")
})
