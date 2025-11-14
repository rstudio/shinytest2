# Launch test event recorder for a Shiny app

Once a recording is completed, it will create or append a new shinytest2
test to the testthat `test_file`.

## Usage

``` r
record_test(
  app = ".",
  ...,
  name = NULL,
  seed = NULL,
  load_timeout = NULL,
  shiny_args = list(),
  test_file = "test-shinytest2.R",
  open_test_file = rlang::is_interactive(),
  allow_no_input_binding = NULL,
  record_screen_size = TRUE,
  run_test = TRUE
)
```

## Arguments

- app:

  A
  [`AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.md)
  object, or path to a Shiny application.

- ...:

  Must be empty. Allows for parameter expansion.

- name:

  Name provided to
  [`AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.md).
  This value should be unique between all tests within a test file. If
  it is not unique, different expect methods may overwrite each other.

- seed:

  A random seed to set before running the app. This seed will also be
  used in the test script.

- load_timeout:

  Maximum time to wait for the Shiny application to load, in
  milliseconds. If a value is provided, it will be saved in the test
  script.

- shiny_args:

  A list of options to pass to
  [`runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). If a value is
  provided, it will be saved in the test script.

- test_file:

  Base file name of the testthat test file.

- open_test_file:

  If `TRUE`, the test file will be opened in an editor via
  [`file.edit()`](https://rdrr.io/r/utils/file.edit.html) before
  executing.

- allow_no_input_binding:

  This value controls if events without input bindings are recorded.

  - If `TRUE`, events without input bindings are recorded.

  - If `FALSE`, events without input bindings are not recorded.

  - If `NULL` (default), if an updated input does not have a
    corresponding input, a modal dialog will be shown asking if unbound
    input events should be recorded.

  See
  [`AppDriver`](https://rstudio.github.io/shinytest2/reference/AppDriver.md)`$set_inputs()`
  for more information.

- record_screen_size:

  If `TRUE`, the screen size will be recorded when initialized and
  changed.

- run_test:

  If `TRUE`, `test_file` will be executed after saving the recording.

## Uploading files

Files that are uploaded to your Shiny app must be located somewhere
within the `tests/testthat` subdirectory or available via
[`system.file()`](https://rdrr.io/r/base/system.file.html).

Files that are uploaded during recording that do not have a valid path
will have a warning inserted into the code. Please fix the file path by
moving the file to the `tests/testthat` subdirectory or by using
[`system.file()`](https://rdrr.io/r/base/system.file.html). After fixing
the path, remove the line of warning code.

## See also

[`test_app()`](https://rstudio.github.io/shinytest2/reference/test_app.md)

## Examples

``` r
if (FALSE) { # \dontrun{
record_test("path/to/app")
} # }
```
