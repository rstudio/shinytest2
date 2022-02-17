# shinytest2 <a href='https://testthat.r-lib.org/'><img src="https://testthat.r-lib.org/logo.png"  align="right" height="138.5" style="margin:10px;" /></a><a href='https://shiny.rstudio.com/'><img src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/shiny.svg"  align="right" height="138.5" style="margin:10px;" /></a>


<!-- badges: start -->
<!-- badges: end -->

`{shinytest2}` facilitates the testing of `{shiny}` applications using a headless Chromium web browser via `{chromote}`. Using the latest features of `{testthat}` edition 3, snapshot files are saved for each of the expected values.

## Installation

You can install the development version of shinytest2 from [GitHub](https://github.com/) with:

``` r
remotes::install_github("rstudio/shinytest2")
```

## Recording example

To record a test for an existing `{shiny}` application, use the `record_test()` method:

```r
shinytest::record_test(".")
```

When a recording is saved, both the test file (`./tests/testthat/test-shinytest2.R`) and specialized test runner (`./tests/testthat/testthat.R`) will be saved to disk.

## Testing example

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


# `{shinytest}`

`{shinytest}` is the predecesor to `{shinytest2}`. `{shinytest}` relied on {webdriver} which uses PhantomJS. PhantomJS has been unsupported since 2017 and does not support displaying Bootstrap v5 via `{bslib}`.

`{shinytest2}` uses {chromote} to connect to your locally installed Chrome or Chromium application. `{shinytest2}` does not have any rendering issues when displaying `{bslib}`'s Bootstrap v5.
