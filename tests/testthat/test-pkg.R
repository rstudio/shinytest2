test_that("installed packages are not treated as dev packages (#451)", {
  withr::local_dir(system.file(package = "testthat"))
  expect_null(dev_pkg_path())
  expect_null(dev_pkg_name())
  expect_false(in_dev_pkg())
})

test_that("source packages are treated as dev packages", {
  withr::local_dir(test_path("pkgs/expkg"))
  expect_equal(dev_pkg_name(), "expkg")
  expect_true(in_dev_pkg())
})
