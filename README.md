# shinytest2 <a href="https://rstudio.github.io/shinytest2/"><img src="man/figures/logo.svg" align="right" height="139" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/shinytest2)](https://CRAN.R-project.org/package=shinytest2)
[![R-CMD-check](https://github.com/rstudio/shinytest2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/shinytest2/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->


> :triangular_flag_on_post::triangular_flag_on_post:<br/>
> {shinytest2} is in beta developement.<br/>
> Please report any bugs or feedback at [https://github.com/rstudio/shinytest2/issues](https://github.com/rstudio/shinytest2/issues) <br/>
> :triangular_flag_on_post::triangular_flag_on_post:


Manually testing Shiny applications is often laborious, inconsistent, and doesn’t scale well. Whether you are developing new features, fixing bug(s), or simply upgrading dependencies on a serious app where mistakes have real consequences, it is critical to know when regressions are introduced. `{shinytest2}` provides a streamlined toolkit for unit testing Shiny applications and seamlessly integrates with the popular [`{testthat}`](https://testthat.r-lib.org/) framework for unit testing R code.

`{shinytest2}` uses [`{chromote}`](https://rstudio.github.io/chromote/) to render applications in a headless Chrome browser. `{chromote}` allows for a live preview, better debugging tools, and/or simply using modern JavaScript/CSS.

By simply recording your actions as code and extending them to test the more particular aspects of your application, it will result in fewer bugs and more confidence in future Shiny application development.


## Installation

You can install the [development version of `{shinytest2}`](https://github.com/rstudio/shinytest2) from GitHub with:

``` r
remotes::install_github("rstudio/shinytest2")
```

Please check out our [Getting Started](https://rstudio.github.io/shinytest2/articles/shinytest2.html) article for example usage.

## Usage

The easiest way to get started is by calling `shinytest2::record_test()` in your app directory. This will open a Shiny application to record your actions as code. To programmatically create a test, call `shinytest2::use_shinytest2_test()`.

Call `shinytest2::use_shinytest2()` to create a initial value test file and set up any other infrastructure you may need.
