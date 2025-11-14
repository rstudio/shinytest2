# Migrate shinytest tests

This function will migrate standard shinytest test files to the new
shinytest2 + testthat ed 3 snapshot format.

## Usage

``` r
migrate_from_shinytest(
  app_dir,
  ...,
  clean = TRUE,
  include_expect_screenshot = missing_arg(),
  quiet = FALSE
)
```

## Arguments

- app_dir:

  Directory containing the Shiny application or Shiny Rmd file

- ...:

  Must be empty. Allows for parameter expansion.

- clean:

  If TRUE, then the shinytest test directory and runner will be deleted
  after the migration to use shinytest2.

- include_expect_screenshot:

  If `TRUE`, `ShinyDriver$snapshot()` will turn into both
  `AppDriver$expect_values()` and `AppDriver$expect_screenshot()`. If
  `FALSE`, `ShinyDriver$snapshot()` will only turn into
  `AppDriver$expect_values()`. If missing, `include_expect_screenshot`
  will behave as `FALSE` if `shinytest::testApp(compareImages = FALSE)`
  or `ShinyDriver$snapshotInit(screenshot = FALSE)` is called.

- quiet:

  Logical that determines if migration information and steps should be
  printed to the console.

## Value

Invisible `TRUE`

## Details

shinytest file contents will be traversed and converted to the new
shinytest2 format. If the shinytest code can not be directly seen in the
code, then it will not be converted.
