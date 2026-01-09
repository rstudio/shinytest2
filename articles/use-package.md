# Using shinytest2 with R packages

For R packages that have Shiny applications, there are generally two
ways that the applications will be present in the package. The first is
to have an `app.R` in a subdirectory of `inst/`. The second way is to
have a function which returns a Shiny app object.

> 🚧 Using [chromote](https://rstudio.github.io/chromote/) in CRAN tests
>
> [chromote](https://rstudio.github.io/chromote/) utilizes the Google
> Chrome application installed on testing machines, which can change
> over time. CRAN has requested that no package utilize
> [chromote](https://rstudio.github.io/chromote/)’s functionality when
> testing on CRAN. This is because the package may not work as expected
> if the version of Google Chrome changes.
>
> To address this, [shinytest2](https://rstudio.github.io/shinytest2/)
> will call
> [`testthat::skip_on_cran()`](https://testthat.r-lib.org/reference/skip.html)
> when creating an `AppDriver` during CRAN testing.
>
> To learn more about CRAN’s reasoning and how to test your package
> using CI, please visit
> [chromote](https://rstudio.github.io/chromote/)’s [Using `{chromote}`
> in CRAN
> tests](https://rstudio.github.io/chromote/articles/example-cran-tests.html).

## Applications defined in another file

Placing your Shiny application in a subdirectory of `inst/` is a common
and clean way to include Shiny applications in R packages.

    /
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R
    ├── inst
    │   └── sample_app
    │       ├── app.R
    └── tests
        ├── testthat
        │   ├── _snaps
        │   │   └── sample_app
        │   │       └── 001.json
        │   └── test-sample_app.R
        └── testthat.R

When recording tests for this application,
[`record_test()`](https://rstudio.github.io/shinytest2/reference/record_test.md)
will save the test files in the package’s `tests/testthat/` directory.
This doesn’t clutter the application directory with test files and
leverages existing package testing infrastructure for a unified testing
experience.

Within each test file, supply the application directory to
`AppDriver$new()`. For example, in `tests/testthat/test-sample_app.R`,
you would have:

``` r

# File: tests/testthat/test-sample_app.R
library(shinytest2)

test_that("sample_app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  appdir <- system.file(package = "exPackage", "sample_app")
  app <- AppDriver$new(appdir, name = "sample_app")

  app$expect_values()
})
```

If the application directory is not meant to be public, it can also be
located in `./tests/testthat/apps`.
[shinytest2](https://rstudio.github.io/shinytest2/) does this with many
application and has `appdir` above point to the relative path to the
application. `app_dir <- testthat::test_path("apps/sample_app")`.

## Applications using a function

Some packages, such as [golem](https://thinkr-open.github.io/golem/),
provide a function that runs or returns a shiny application. In this
case, you can create a wrapper function to perform two actions: load the
package via [`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html) and returns/runs an
app. For example, if your package is named `mypkg`, your package
structure may look like:

    /
    ├── .Rbuildignore
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R
    │   └── run-app.R
    └── tests
        ├── testthat
        │   ├── _snaps
        │   │   └── app-function
        │   │       └── 001.json
        │   └── test-app-function.R
        └── testthat.R

and the function that runs the app is `run_app()`, you can pass in
functions defined within your R package directly to `AppDriver$new()`:

``` r

# File: tests/testthat/test-app-function.R
library(shinytest2)

test_that("mypkg app initial values are consistent", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  app <- AppDriver$new(run_app, name = "mypkg-app")

  app$expect_values()
})
```

This works as [shinytest2](https://rstudio.github.io/shinytest2/)
recognizes that the `run_app` function is part of the local package.
When running a local package methods,
[shinytest2](https://rstudio.github.io/shinytest2/) will call the method
within the scope of your local R package in the the background process.

The specially handled function is functionally equivalent to defining a
wrapper function that loads the package and calls `run_app()`:

``` r

# File: tests/testthat/test-app-function.R
library(shinytest2)
run_mypkg_app <-

test_that("mypkg app initial values are consistent", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  app <- AppDriver$new(name = "mypkg-app", function() {
    library(mypkg)
    run_app()
  })

  app$expect_values()
})
```

When using a function to initialize the `AppDriver`,
[shinytest2](https://rstudio.github.io/shinytest2/) will automatically
call
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
when you execute `library(<pkg>)` or `require(<pkg>)` to load your dev
package, `<pkg>`, in the background R process. Currently, only the local
dev package can be loaded. Similar to [mirai](https://mirai.r-lib.org),
there is also no carryover from the testing environment to the supplied
function execution within the background R process. All packages may
need to be [`library()`](https://rdrr.io/r/base/library.html)’ed again!

## Application objects

The third way have an application in an R package is by having a
function that returns a Shiny application object. App objects being
supplied directly to `AppDriver$new()` are to be considered for quick
testing / confirmation of targeted behavior. For more complex
applications, it is recommended to use one of the other two approaches
described above as not all global variables and contexts are preserved
when serializing the app object.

In this example, there’s a function `hello_world_app()`, which lives in
`R/app-object.R`:

    /
    ├── .Rbuildignore
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R
    │   └── app-object.R
    └── tests
        ├── testthat
        │   ├── _snaps
        │   │   └── app-object
        │   │       └── 001.json
        │   └── test-app-object.R
        └── testthat.R

The function simply returns an object from
[`shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html):

``` r

# File: R/app-object.R

dt <- datasets::cars

hello_world_app <- function() {
  shinyApp(
    ui = fluidPage(
      sliderInput("n", "n", 1, nrow(dt), 10),
      plotOutput("plot")
    ),
    server = function(input, output) {
      output$plot <- renderPlot({
        plot(
          head(dt, input$n),
          xlim = range(dt[[1]]),
          ylim = range(dt[[2]])
        )
      })
    }
  )
}
```

Once we have the object, it can be supplied directly to
`AppDriver$new()`.

``` r

# File: tests/testthat/test-app-function.R

test_that("hello-world app initial values are consistent", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  shiny_app <- hello_world_app()
  app <- AppDriver$new(shiny_app, name = "hello")

  app$expect_values()
})
```

To help create tests, you can call
[`record_test()`](https://rstudio.github.io/shinytest2/reference/record_test.md)
on your shiny application object directly. Unfortunately, the test file
will not be able to be saved. Instead, the test commands can be copied
into a test script manually.

## Other setup steps

There are a few steps that are needed for both types of tests.

It is recommended to call
[`shinytest2::use_shinytest2()`](https://rstudio.github.io/shinytest2/reference/use_shinytest2.md)
to enable different test config set-ups.

You will need to add [shinytest2](https://rstudio.github.io/shinytest2/)
to the `Suggests` section in your `DESCRIPTION` file.

    Suggests:
        shinytest2

When all of these items are in place, you can test your package using
[`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html)
or by running `R CMD check` on your package. If you are using the
RStudio IDE, you can also run Build -\> Test Package or Build -\> Check
Package.

[shinytest2](https://rstudio.github.io/shinytest2/) **no longer**
requires that your package to be *installed* when testing. It will load
from your local path.
[`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html)
(and related wrappers) eventually call
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
to temporarily source the local R package in the foreground for unit
testing. [shinytest2](https://rstudio.github.io/shinytest2/) has made
the stance that App authors **must** call `library(<pkg>)` or
`require(<pkg>)` on their package within their App. This ensures that
the package is loaded in the background R process that runs the Shiny
application. When `library*()` is called,
[shinytest2](https://rstudio.github.io/shinytest2/) will automatically
execute
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
to load the package’s source code.

## How should I test multiple applications?

Given you have multiple applications / app functions in your package,
you can create multiple test files in `tests/testthat/`, one for each
application. Each test file can create its own `AppDriver` instance for
the specific application being tested.

It is not recommended to utilize `test_app("path/to/app")` within
package testing as we want the tests to be controlled by the package’s
`testthat` infrastructure. This allows for more flexibility and control
over how your applications are tested while your current package’s
testthat infrastructure.

Good: \* Supply a function from within your package to `AppDriver$new()`
and test within your package’s
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
tests. \* Supply an application directory within your package to
`AppDriver$new()` and test within your package’s
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
tests. Bad: \* Call `test_app("path/to/app")` within your package’s
[`testthat::test_that()`](https://testthat.r-lib.org/reference/test_that.html)
tests.

This keeps your snaps all in one place and not scattered across multiple
application directories.

## Migrating from shinytest v0.4.0 to v0.5.0

Previously, it was encouraged to store tests within the application
directory itself. This made sense when applications were primarily
standalone. However, for R packages, it is more idiomatic to store tests
in the `tests/testthat/` directory and utilize an app that is defined
elsewhere (either in `inst/` or another directory).

Previous file structure:

    /
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R
    ├── inst
    │   └── sample_app
    │       ├── app.R
    │       ├── R
    │       │   └── helpers.R
    │       └── tests
    │           ├── testthat.R
    │           └── testthat
    │              ├── setup-shinytest2.R
    │              └── test-sample_app.R
    └── tests
        ├── testthat.R
        └── testthat
            └── test-other-things.R

Previous `test-sample_app.R`:

``` r

library(shinytest2)

test_that("sample_app works", {
  app <- AppDriver$new(name = "sample_app")

  app$expect_values()
  expect_equal(app$get_value("input$n"), helper_value)
})
```

It is now recommended to store all tests in `tests/testthat/` and have
the application defined elsewhere. This keeps the application directory
clean and leverages existing package testing infrastructure for a
unified testing experience.

New file structure:

    /
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R
    ├── inst
    │   └── sample_app
    │       ├── app.R
    │       ├── R
    │           └── helpers.R
    └── tests
        ├── testthat.R
        └── testthat
            ├── test-other-things.R
            └── test-sample_app.R

New `test-sample_app.R`:

``` r

library(shinytest2)
test_that("sample_app works", {
  appdir <- system.file(package = "exPackage", "sample_app")
  local_app_support(appdir)

  app <- AppDriver$new(appdir, name = "sample_app")

  app$expect_values()
  expect_equal(app$get_value("input$n"), helper_value)
})
```

Since we have moved the tests out of the application directory, we need
to ensure that any supporting files (like snapshots) are still
accessible. When testing **only** an application, the premise is similar
to a package where all supporting files should be available in the
testing environment. However, within a package, the application is a
means to test the package and should not automatically load supporting
files into the entire testing environment, possibly causing conflicts
with other Shiny applications or tests.

To address this, we use `local_app_support(appdir)` to temporarily link
the necessary support files from the application directory to the test
environment. This ensures that the tests can access the required
snapshots and other supporting files without cluttering the application
directory itself.

## Continuous integration

If you would like your package to be tested with every commit, you can
set it up with GitHub Actions. Please see [Using shinytest2 with
continuous
integration](https://rstudio.github.io/shinytest2/articles/use-ci.md)
for inspiration.
