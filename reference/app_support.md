# Attach the Shiny application's support environment

Executes all `./R` files and `global.R` into a temp environment that is
attached appropriately. This is useful when wanting access to functions
or values created in the `./R` folder for testing purposes.

## Usage

``` r
local_app_support(app_dir, envir = rlang::caller_env())

with_app_support(app_dir, expr, envir = rlang::caller_env())

load_app_support(app_dir, envir = rlang::caller_env())
```

## Arguments

- app_dir:

  The base directory for the Shiny application.

- envir:

  The environment in which the App support should be made available.

- expr:

  An expression to evaluate within the support environment.

## Details

For Shiny application testing within R packages, `local_app_support()`
and `with_app_support()` where loading an App's support files should not
happen automatically.

For non-package based Shiny applications, it is recommended to use
`load_app_support()` for the support to be available throughout all test
files.

## Functions

- `local_app_support()`: Temporarily attach the Shiny application's
  support environment into the current environment.

- `with_app_support()`: For the provided `expr`, attach the Shiny
  application's support environment into the current environment.

- `load_app_support()`: Loads all support files into the current
  environment. No cleanup actions are ever performed.

## Examples

``` r
if (FALSE) { # \dontrun{
# ./tests/testthat/apps/myapp/R/utils.R
n <- 42

#' # ./tests/testthat/test-utils.R
test_that("Can access support environment", {
  expect_false(exists("n"))
  shinytest2::local_app_support(test_path("apps/myapp"))
  expect_equal(n, 42)
})

# Or using with_app_support()
test_that("Can access support environment", {
  expect_false(exists("n"))
  shinytest2::with_app_support(test_path("apps/myapp"), {
    expect_equal(n, 42)
  })
  expect_false(exists("n"))
})
} # }
```
