# shinytest2

Manually testing Shiny applications is often laborious, inconsistent,
and doesn’t scale well. Whether you are developing new features, fixing
bug(s), or simply upgrading dependencies on a serious app where mistakes
have real consequences, it is critical to know when regressions are
introduced. [shinytest2](https://rstudio.github.io/shinytest2/) provides
a streamlined toolkit for unit testing Shiny applications and seamlessly
integrates with the popular [testthat](https://testthat.r-lib.org)
framework for unit testing R code.

[shinytest2](https://rstudio.github.io/shinytest2/) uses
[chromote](https://rstudio.github.io/chromote/) to render applications
in a headless Chrome browser.
[chromote](https://rstudio.github.io/chromote/) allows for a live
preview, better debugging tools, and/or simply using modern
JavaScript/CSS.

By simply recording your actions as code and extending them to test the
more particular aspects of your application, it will result in fewer
bugs and more confidence in future Shiny application development.

## Installation

``` r

# Install the released version from CRAN
install.packages("shinytest2")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("rstudio/shinytest2")
```

## Usage

The easiest way to get started is by calling
[`shinytest2::record_test()`](https://rstudio.github.io/shinytest2/reference/record_test.md)
in your app directory. This will open a Shiny application to record your
actions as code. To programmatically create a test, call
[`shinytest2::use_shinytest2_test()`](https://rstudio.github.io/shinytest2/reference/use_shinytest2.md).

Call
[`shinytest2::use_shinytest2()`](https://rstudio.github.io/shinytest2/reference/use_shinytest2.md)
to create a initial value test file and set up any other infrastructure
you may need.

Please check out our [Getting
Started](https://rstudio.github.io/shinytest2/articles/shinytest2.html)
article for example usage.
