# Attach the Shiny application's support environment

Executes all `./R` files and `global.R` from a Shiny application
directory into an environment that is attached appropriately. This is
useful when you need access to helper functions, modules, or values
created in the `./R` folder for testing purposes.

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

## Functions

- `local_app_support()`: Temporarily attach the Shiny application's
  support environment into the current environment.

- `with_app_support()`: For the provided `expr`, attach the Shiny
  application's support environment into the current environment.

- `load_app_support()`: Loads all support files into the current
  environment. No cleanup actions are ever performed.

## When to use these functions

**For inline apps (app objects passed directly to AppDriver)**: You do
**NOT** need these functions. Since your test file already has access to
your package functions (via
[`library(yourpackage)`](https://rdrr.io/r/base/library.html) or
`devtools::load_all()`), you can reference them directly when creating
the app object.

**For apps with separate support files**: Use these functions when your
app is stored in a directory (e.g., `inst/myapps/app1/`) and has
supporting R files in an `app.R` / `server.R`, `global.R, and `./R\`
subfolder that you need to access in your tests.

## Choosing which function to use

- **`local_app_support()`**: For R packages or when you want automatic
  cleanup. The support environment is attached for the duration of the
  current environment, then automatically removed. This prevents
  pollution of the testing environment across multiple tests that test
  multiple apps.

- **`with_app_support()`**: Similar to `local_app_support()`, but uses
  an explicit code block. The support environment is only available
  within the provided expression and is cleaned up immediately after.

- **`load_app_support()`**: For standalone Shiny apps (non-package).
  Loads support files into the testing environment permanently with no
  automatic cleanup. Best used in `setup-shinytest2.R` for non-package
  apps.

## Benefits

- **Isolation**: `local_app_support()` and `with_app_support()` prevent
  helper functions from one app test from interfering with another.

- **Access to app internals**: Test helper functions, modules, and
  utilities defined in your app's `./R` folder without manually sourcing
  files.

- **Package testing**: Enables testing apps within packages where the
  app's support files are separate from the package's exported
  functions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic usage with local_app_support()
# File structure:
#   ./tests/testthat/apps/myapp/R/utils.R  (contains: n <- 42)
#   ./tests/testthat/test-utils.R

test_that("Can access support environment", {
  expect_false(exists("n"))
  shinytest2::local_app_support(test_path("apps/myapp"))
  expect_equal(n, 42)
})
# After test completes, n is automatically removed

# Example 2: Using with_app_support() for explicit scoping
test_that("Can access support environment", {
  expect_false(exists("n"))
  shinytest2::with_app_support(test_path("apps/myapp"), {
    expect_equal(n, 42)
  })
  expect_false(exists("n"))  # n is no longer available
})

# Example 3: Testing an app with its support files
# File structure:
#   ./inst/myapps/app1/app.R
#   ./inst/myapps/app1/R/modules.R  (contains: my_module_ui <- function() {...})
#   ./tests/testthat/test-app1.R

test_that("app1 loads with module support", {
  expect_false(exists("my_module_ui"))

  app_dir <- system.file("myapps/app1", package = "mypkg")
  local_app_support(app_dir)

  # Now we can access module functions from app1/R/
  expect_true(exists("my_module_ui"))

  # And test the app
  app <- AppDriver$new(app_dir)
  app$expect_values()
})
} # }
```
