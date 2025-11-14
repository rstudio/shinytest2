# Load the Shiny application's support environment

Executes all `./R` files and `global.R` into the current environment.
This is useful when wanting access to functions or values created in the
`./R` folder for testing purposes.

## Usage

``` r
load_app_env(
  app_dir = "../../",
  renv = rlang::caller_env(),
  globalrenv = rlang::caller_env()
)
```

## Arguments

- app_dir:

  The base directory for the Shiny application.

- renv:

  The environment in which the files in the \`R/“ directory should be
  evaluated.

- globalrenv:

  The environment in which `global.R` should be evaluated. If `NULL`,
  `global.R` will not be evaluated at all.

## Details

Loading these files is not automatically performed by
[`test_app()`](https://rstudio.github.io/shinytest2/reference/test_app.md)
and must be called in `./tests/testthat/setup-shinytest2.R` if access to
support file objects is desired.

## See also

[`use_shinytest2()`](https://rstudio.github.io/shinytest2/reference/use_shinytest2.md)
for creating a testing setup file that loads your Shiny app support
environment into the testing environment.
