expect_configs <- function(runner, setup, ignore, package) {
  if (all(!runner, !setup, !ignore, !package)) {
    testthat::expect_error(
      use_shinytest2(
        app_dir = ".",
        runner = FALSE,
        setup = FALSE,
        ignore = FALSE,
        package = FALSE
      ),
      "At least one of",
      fixed = TRUE
    )
    return()
  }

  app_dir <- tempfile("st2-test")
  fs::dir_create(app_dir)
  withr::defer(fs::dir_delete(app_dir))

  if (package || ignore) {
    # Set up DESCRIPTION file
    write.dcf(
      list(
        "Package" = "testshinytest2",
        "Title" = "Test for ShinyTest2"
      ),
      fs::path(app_dir, "DESCRIPTION")
    )
  }
  if (ignore) {
    # Set up .Rbuildignore file
    cat("", file = fs::path(app_dir, ".Rbuildignore"))
  }

  info <- paste0(
    "runner = ",
    runner,
    "\n",
    "setup = ",
    setup,
    "\n",
    "ignore = ",
    ignore,
    "\n",
    "package = ",
    package
  )

  capture.output(
    {
      use_shinytest2(
        app_dir = app_dir,
        runner = runner,
        setup = setup,
        ignore = ignore,
        package = package
      )
    },
    type = "message"
  )

  testthat::expect_equal(
    file.exists(file.path(app_dir, "tests/testthat.R")),
    runner,
    info = info
  )

  testthat::expect_equal(
    file.exists(file.path(app_dir, "tests/testthat/setup-shinytest2.R")),
    setup,
    info = info
  )

  testthat::expect_equal(
    file.exists(file.path(app_dir, ".gitignore")),
    ignore,
    info = info
  )
  testthat::expect_equal(
    file.exists(file.path(app_dir, ".Rbuildignore")),
    ignore,
    info = info
  )

  testthat::expect_equal(
    file.exists(file.path(app_dir, "DESCRIPTION")),
    package || ignore,
    info = info
  )
  if (file.exists(file.path(app_dir, "DESCRIPTION"))) {
    suggests <- as.list(read.dcf(fs::path(app_dir, "DESCRIPTION"))[
      1,
    ])$Suggests
    wrapper <- if (package) {
      testthat::expect_success
    } else {
      testthat::expect_failure
    }
    wrapper(
      testthat::expect_equal(suggests, "shinytest2", info = info)
    )
  }
}

test_that("use_shinytest2() sets up configs for all configurations", {
  skip_if_not_installed("usethis")

  dt <- expand.grid(
    runner = c(TRUE, FALSE),
    setup = c(TRUE, FALSE),
    ignore = c(TRUE, FALSE),
    package = c(TRUE, FALSE)
  )
  Map(
    dt$runner,
    dt$setup,
    dt$ignore,
    dt$package,
    f = expect_configs
  )
})

test_that("use_shinytest2_setup() creates a file with no warnings when being read", {
  temp_dir <- tempfile("st2-test")
  fs::dir_create(temp_dir)

  expect_warning(
    {
      use_shinytest2_setup(temp_dir, quiet = TRUE)
      readLines(file.path(temp_dir, "tests/testthat/setup-shinytest2.R"))
    },
    NA
  )
})
