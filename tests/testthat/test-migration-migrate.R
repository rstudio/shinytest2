# Too many possibilities for tests to randomly fail (e.g. file paths on windows, etc.)
# The other test files cover this fairly well. CI is good enough
skip_on_cran()


expect_migration <- function(test_app_folder, ...) {
  original_path <- testthat::test_path(test_app_folder)
  if (!fs::dir_exists(original_path)) {
    testthat::skip(paste0("Migration app not found: ", test_app_folder))
  }
  new_path <- tempfile()
  fs::dir_copy(original_path, new_path)

  migrate_from_shinytest(new_path, ..., quiet = TRUE)

  expected_path <- paste0(original_path, "ex")

  expected_files <- fs::dir_ls(expected_path, recurse = TRUE)
  new_files <- fs::dir_ls(new_path, recurse = TRUE)

  testthat::expect_equal(
    sort_c(fs::path_rel(new_files, new_path)),
    sort_c(fs::path_rel(expected_files, expected_path))
  )
  fs::dir_walk(new_path, recurse = TRUE, fun = function(new_file_path) {
    if (fs::dir_exists(new_file_path)) {
      return()
    }
    new_rel_path <- fs::path_rel(new_file_path, new_path)
    x_arg <- paste0(
      fs::path(fs::path_file(test_app_folder), new_rel_path),
      " file contents"
    )
    y_arg <- paste0(
      fs::path(fs::path_file(expected_path), new_rel_path),
      " file contents"
    )
    label <- paste0(x_arg, " equals ", y_arg)
    expected_file_path <- fs::path(
      expected_path,
      fs::path_rel(new_file_path, new_path)
    )
    switch(
      fs::path_ext(new_file_path),
      "png" = testthat::expect_true(
        testthat::compare_file_binary(new_file_path, expected_file_path),
        label = label
      ),
      testthat::expect_equal(
        strsplit(read_utf8(new_file_path), "\n")[[1]],
        strsplit(read_utf8(expected_file_path), "\n")[[1]],
        label = x_arg,
        expected.label = y_arg
      )
    )
  })
}


test_that("Migrations work", {
  expect_migration("migrate-apps/01")
  expect_migration("migrate-apps/02")
  expect_migration("migrate-apps/05")
  expect_migration("migrate-apps/08")
  expect_migration("migrate-apps/09")
  expect_migration("migrate-apps/10")
})
