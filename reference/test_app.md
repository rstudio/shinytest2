# Test Shiny applications with testthat

This is a helper method that wraps around
[`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
to test your Shiny application or Shiny runtime document. This is
similar to how
[`testthat::test_check()`](https://testthat.r-lib.org/reference/test_package.html)
tests your R package but for your app.

## Usage

``` r
test_app(
  app_dir = missing_arg(),
  ...,
  name = missing_arg(),
  reporter = testthat::get_reporter(),
  stop_on_failure = missing_arg(),
  check_setup = deprecated(),
  quiet = FALSE
)
```

## Arguments

- app_dir:

  The base directory for the Shiny application.

  - If `app_dir` is missing and `test_app()` is called within the
    `./tests/testthat.R` file, the parent directory (`"../"`) is used.

  - Otherwise, the default path of `"."` is used.

- ...:

  Parameters passed to
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)

- name:

  Name to display in the middle of the test name. This value is only
  used when calling `test_app()` inside of testhat test. The final
  testing context will have the format of
  `"{test_context} - {name} - {app_test_context}"`.

- reporter:

  Reporter to pass through to
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html).

- stop_on_failure:

  If missing, the default value of `TRUE` will be used. However, if
  missing and currently testing, `FALSE` will be used to seamlessly
  integrate the app reporter to `reporter`.

- check_setup:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html). Parameter
  ignored.

- quiet:

  If `TRUE`, suppresses deprecation warnings when called within testthat
  tests.

## Details

Example usage:

    ## Interactive usage
    # Test Shiny app in current working directory
    shinytest2::test_app()

    # Test Shiny app in another directory
    path_to_app <- "path/to/app"
    shinytest2::test_app(path_to_app)

    ## File: ./tests/testthat.R
    # Will find Shiny app in "../"
    shinytest2::test_app()

    ## File: ./tests/testthat/test-shinytest2.R
    # Test a shiny application within your own {testthat} code
    test_that("Testing a Shiny app in a package", {
      app <- shinytest2::AppDriver$new(path_to_app)
      # Perform tests with `app`...
    })

When testing within a package, it is recommended to not call
`test_app()`, but instead test your applications within your own
testthat tests. This allows for more flexibility and control over how
your applications are tested while your current package's testthat
infrastructure. See the [Use Package
vignette](https://rstudio.github.io/shinytest2/articles/use-package.html)
for more details.

## Uploading files

When testing an application, all non-temp files that are uploaded should
be located in the `./tests/testthat` directory. This allows for tests to
be more portable and self contained.

When recording a test with
[`record_test()`](https://rstudio.github.io/shinytest2/reference/record_test.md),
for every uploaded file that is located outside of `./tests/testthat`, a
warning will be thrown. Once the file path has be fixed, you may remove
the warning statement.

## Different ways to test

`test_app()` is an opinionated testing function that will only execute
testthat tests in the `./tests/testthat` folder. If (for some rare
reason) you have other non-testthat tests to execute, you can call
[`shiny::runTests()`](https://rdrr.io/pkg/shiny/man/runTests.html). This
method will generically run all test runners and their associated tests.

    # Execute a single Shiny app's {testthat} file such as `./tests/testthat/test-shinytest2.R`
    test_app(filter = "shinytest2")

    # Execute all {testthat} tests
    test_app()

    # Execute all tests for all test runners
    shiny::runTests()

## See also

- [`record_test()`](https://rstudio.github.io/shinytest2/reference/record_test.md)
  to create tests to record against your Shiny application.

- [`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html)
  and
  [`testthat::snapshot_accept()`](https://testthat.r-lib.org/reference/snapshot_accept.html)
  if you want to compare or update snapshots after testing.

- [`local_app_support()`](https://rstudio.github.io/shinytest2/reference/app_support.md)
  /
  [`with_app_support()`](https://rstudio.github.io/shinytest2/reference/app_support.md)
  to load the Shiny application's helper files into respective
  environments. These methods are useful for within package testing as
  they have fine tune control over when the support environment is
  loaded.

- [`load_app_support()`](https://rstudio.github.io/shinytest2/reference/app_support.md)
  to load the Shiny application's helper files into the calling
  environment. This method is useful for non-package based Shiny
  applications where the support environment should be available in
  every test file.
