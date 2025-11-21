# Use shinytest2 with your Shiny application

Use shinytest2 with your Shiny application

## Usage

``` r
use_shinytest2(
  app_dir = ".",
  runner = missing_arg(),
  setup = missing_arg(),
  ignore = missing_arg(),
  package = missing_arg(),
  ...,
  quiet = FALSE,
  overwrite = FALSE
)

use_shinytest2_test(
  app_dir = ".",
  open = rlang::is_interactive(),
  quiet = FALSE,
  overwrite = FALSE
)
```

## Arguments

- app_dir:

  The base directory for the Shiny application

- runner:

  If `TRUE`, creates a shinytest2 test runner at `./tests/testthat.R`

- setup:

  If `TRUE`, creates a setup file called
  `./tests/testthat/setup-shinytest2.R` containing a call to
  [`load_app_support()`](https://rstudio.github.io/shinytest2/reference/app_support.md).
  If you would like fine grain control over when the environment is
  loaded, please look at
  [`local_app_support()`](https://rstudio.github.io/shinytest2/reference/app_support.md)
  and
  [`with_app_support()`](https://rstudio.github.io/shinytest2/reference/app_support.md).

- ignore:

  If `TRUE`, adds entries to `.Rbuildignore` and `.gitignore` to ignore
  new debug screenshots. (`*_.new.png`)

- package:

  If `TRUE`, adds shinytest2 to `Suggests` in the `DESCRIPTION` file.

- ...:

  Must be empty. Allows for parameter expansion.

- quiet:

  If `TRUE`, console output will be suppressed.

- overwrite:

  If `TRUE`, the test file or test runner will be overwritten.

- open:

  If `TRUE`, the test file will be opened in an editor via
  [`file.edit()`](https://rdrr.io/r/utils/file.edit.html) after saving.

## Functions

- `use_shinytest2()`: This usethis-style method initializes many
  different useful features when using shinytest2:

  - `runner`: Creates a shinytest2 test runner at `./tests/testthat.R`.
    This file will contain a call to
    [`test_app()`](https://rstudio.github.io/shinytest2/reference/test_app.md).

  - `setup`: Creates `./tests/testthat/setup-shinytest2.R` to add your
    Shiny `./R` objects and functions into the testing environment. This
    file will run before testing begins.

  - `ignore`: Add an entry to `./Rbuildignore` (if it exists) and
    `.gitignore` to ignore new debug screenshots. (`*_.new.png`)

  - `package`: Adds `shinytest` to the `Suggests` packages in the
    `DESCRIPTION` file (if it exists).

  When all values are missing and currently in a package working
  directory, the defaults are all TRUE. When the current working
  directory is a package root directory, `runner`/`setup` are `FALSE`
  and `ignore`/`package` are `TRUE`.

  If any of these values are *not* missing, the remaining missing values
  will be set to `FALSE`. This allows `use_shinytest2()` to add more
  flags in future versions without opting into all changes
  inadvertently.

- `use_shinytest2_test()`: Creates a test file called
  `./tests/testthat/test-shinytest2.R`. By default, this file's template
  test will initialize your Shiny application and expect the initial
  values.

  This method will also set up a test runner if it does not exist.

## Examples

``` r
# Set up shinytest2 testing configs
if (FALSE) use_shinytest2() # \dontrun{}
# Set up a shinytest2 test
if (FALSE) use_shinytest2_test() # \dontrun{}
```
