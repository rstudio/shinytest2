# shinytest2


<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/shinytest2)](https://CRAN.R-project.org/package=shinytest2)
[![R-CMD-check](https://github.com/rstudio/shinytest2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/shinytest2/actions)
<!-- badges: end -->


> !! {shinytest2} is in beta developement. Please report any bugs or feedback at [https://github.com/rstudio/shinytest2/issues](https://github.com/rstudio/shinytest2/issues) !!


`{shinytest2}` facilitates the testing of `{shiny}` applications using a headless Chromium web browser via `{chromote}`. Using the latest features of `{testthat}` edition 3, snapshot files are saved for each of the expected values.

## Installation

You can install the development version of shinytest2 from [GitHub](https://github.com/) with:

``` r
remotes::install_github("rstudio/shinytest2")
```

## Recording a test

To record a test for an existing `{shiny}` application, use the `record_test()` method:

```r
shinytest::record_test(".")
```

When a recording is saved, both the test file (`./tests/testthat/test-shinytest2.R`) and specialized test runner (`./tests/testthat/testthat.R`) will be saved to disk.

## Test example

The saved recording will look similar to

``` r
# ./tests/testthat/test-shinytest2.R
library(shinytest2)

test_that("values are captured", {
  app <- AppDriver$new(name = "unique-name")
  app$expect_values()
})
```

The call to `app$expect_values()` will save an expected snapshot to `./tests/testthat/_snaps/shinytest2/unique-name-001.json`.

In addition to the expected snapshot, a debug screenshot file will be saved to `./tests/testthat/_snaps/shinytest2/unique-name-001_.png`. These screenshot files should be kept in version control (`GitHub`) to see how your app updates over times, but if visual differences are found, these differences will never fail a test.


# Migrating from `{shinytest}`

`{shinytest}` is the predecesor to `{shinytest2}`. `{shinytest}` was implemented using `{webdriver}` which uses [PhantomJS](https://phantomjs.org/api/). PhantomJS has been unsupported since 2017 and does not support displaying `{bslib}`'s Bootstrap v5.

`{shinytest2}` uses {chromote} to connect to your locally installed Chrome or Chromium application. `{shinytest2}` does not have any rendering issues when displaying `{bslib}`'s Bootstrap v5.

To migrate your existing `{shinytest}` tests to `{shinytest2}`, call the helper method `shinytest2::migrate_from_shinytest(path)`.
