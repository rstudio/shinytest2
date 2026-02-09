# Robust testing

All tests are not created equal. Some tests are all encompassing; others
are more focused. Each approach has its own advantages and
disadvantages.

Goals to have when writing tests are to

- Confirm the expected behavior
- Assert as little unnecessary information
- Write clear, direct tests

## Expectations

Unit tests provide a sense of security by making assertions on expected
behavior. If all unit tests pass, then we can declare the object/method
to be valid. 🦆

Let’s look at a quick example of:

``` r

add_abs <- function(x, y) {
  abs(x + y)
}
```

##### Confirm the expected behavior

If we only test positive numbers, then we can guess the answer using
`+`.

``` r

x <- 50; y <- 42
testthat::expect_equal(add_abs(x, y), x + y)
```

However, if a negative number is used, it will have different behavior
than `+`. So we must test for both situations.

``` r

x <- 50; y <- -42
testthat::expect_equal(add_abs(x, y), x + y)
```

Even better, let’s test all four positive/negative situations:

``` r

for (info in list(
  list(x =  50, y =  42, expected =  92),
  list(x = -50, y =  42, expected =  8),
  list(x =  50, y = -42, expected =  8),
  list(x = -50, y = -42, expected =  92)
)) {
  testthat::expect_equal(
    add_abs(info$x, info$y),
    info$expected
  )
}
```

This concept can even be expanded to vectors of values. However, this
can lead to a lot of code.

##### Assert as little unnecessary information

Tests should strive to only confirm behavior that we have control over.
For example, if we are only interested in the behavior of `abs`, then we
should not be concerned with the behavior of `+`.

We can assume that `+` is working properly and adds two numbers
together. We do not necessarily need to perform a multitude of nonsense
input testing on `+`, but it may make sense to see how
[`abs()`](https://rdrr.io/r/base/MathFun.html) handles a few non-number
input values.

``` r

testthat::expect_equal(add_abs(1, NA), NA)
testthat::expect_equal(add_abs(1, NULL), numeric(0))
testthat::expect_error(add_abs(1, "string"))
```

These tests could also be repeated by swapping `x` and `y`.

##### Write clear, direct tests

When writing tests, each test can contain many expectations but each
expectation should pertain to the test being run.

For example, the two examples above *could* be included in the same test
block:

``` r

# File: tests/testthat/test-add_abs-bad.R

test_that("add_abs() works", {
  for (info in list(
    list(x =  50, y =  42, expected =  92),
    list(x = -50, y =  42, expected =  8),
    list(x =  50, y = -42, expected =  8),
    list(x = -50, y = -42, expected =  92)
  )) {
    expect_equal(
      add_abs(info$x, info$y),
      info$expected
    )
  }

  expect_equal(add_abs(1, NA), NA)
  expect_equal(add_abs(1, NULL), numeric(0))
  expect_error(add_abs(1, "string"))
})
```

However, it’s better to break them out into two separate tests, each
with their own descriptive title.

``` r

# File: tests/testthat/test-add_abs-better.R

test_that("add_abs() adds two numbers", {
  for (info in list(
    list(x =  50, y =  42, expected =  92),
    list(x = -50, y =  42, expected =  8),
    list(x =  50, y = -42, expected =  8),
    list(x =  50, y = -42, expected =  -8),
    list(x = -50, y = -42, expected =  92)
  )) {
    expect_equal(
      add_abs(info$x, info$y),
      info$expected
    )
  }
})

test_that("add_abs() handles non-numeric input", {
  expect_equal(add_abs(1, NA), NA)
  expect_equal(add_abs(1, NULL), numeric(0))
  expect_error(add_abs(1, "string"))
})
```

[testthat](https://testthat.r-lib.org) does a great job of displaying
output to the user when thing go wrong. When the first test goes wrong,
an error will be given like:

    ── Failure (Line 9): add_abs() adds two numbers ────────────────────────────────
    add_abs(info$x, info$y) (`actual`) not equal to info$expected (`expected`).

      `actual`:  8
    `expected`: -8

It is great that an error was found, but it is also difficult to
determine which assertion failed. Adding labels to the expectation using
`label` and `expected.label` allows you to provide more context about
which test failed.

``` r

# File: tests/testthat/test-add_abs-label.R

test_that("add_abs() adds two numbers", {
  for (info in list(
    list(x =  50, y =  42, expected =  92),
    list(x = -50, y =  42, expected =  8),
    list(x =  50, y = -42, expected =  8),
    list(x =  50, y = -42, expected =  -8), # <- Failing line
    list(x = -50, y = -42, expected =  92)
  )) {
    expect_equal(
      add_abs(info$x, info$y),
      info$expected,
      label = paste0("x:", info$x, "; y:", info$y),
      expected.label = info$expected
    )
  }
})
#> ── Failure (Line 9): add_abs() adds two numbers ────────────────────────────────
#> x:50; y:-42 (`actual`) not equal to info$expected (`expected`).
#>
#>   `actual`:  8
#> `expected`: -8
```

Another pattern that provides more context and allows for more tests is
to move the for-loop around the call to
[`test_that()`](https://testthat.r-lib.org/reference/test_that.html) and
give the test a custom name:

``` r

# File: tests/testthat/test-add_abs-label.R

for (info in list(
  list(x =  50, y =  42, expected =  92),
  list(x = -50, y =  42, expected =  8),
  list(x =  50, y = -42, expected =  8),
  list(x =  50, y = -42, expected =  -8), # <- Failing line
  list(x = -50, y = -42, expected =  92)
)) {
  test_that(paste0("add_abs() adds two numbers: [", info$x, ", ", info$y, "]"), {
    expect_equal(
      add_abs(info$x, info$y),
      info$expected
    )
  })
}
#> Test passed 🎊
#> Test passed 🎉
#> Test passed 🌈
#> ── Failure (Line 9): add_abs() adds two numbers: [50, -42] ─────────────────────
#> add_abs(info$x, info$y) (`actual`) not equal to info$expected (`expected`).
#>
#>   `actual`:  8
#> `expected`: -8
#> Test passed 🥇
```

This isolates the test even more and does not let an earlier expectation
stop testing a later expectation.

## `{shinytest2}` expectations

[shinytest2](https://rstudio.github.io/shinytest2/) has a handful of
built in expectation methods:

- `input`/`output` names:
  - `AppDriver$expect_unique_names()`: Assert all `input` and `output`
    names are unique
- Shiny value expectations:
  - `AppDriver$expect_values()`: Expect all `input`, `output`, and
    `export` values are consistent
  - `AppDriver$expect_download()`: Expect a downloaded file to
    downloadable
- UI expectations:
  - `AppDriver$expect_text()`: Expect the text content for a given
    `selector` to be consistent
  - `AppDriver$expect_html()`: Expect the HTML content for a given
    `selector` to be consistent
  - `AppDriver$expect_js()`: Expect the JavaScript return value to be
    consistent
- UI visual expectations:
  - `AppDriver$expect_screenshot()`: Expect a screenshot of the UI to be
    consistent

### `input`/`output` names

Let’s take a look at `AppDriver$expect_unique_names()`. This method is
called (by default) by `AppDriver$new(check_names = TRUE)` and confirms
that no `input` or `output` HTML names are duplicated. If duplicate
values are found, this results in invalid HTML and possible failure
within Shiny.

One way to help keep `input` and `output` names unique is to adopt a
naming behavior such as adding `_out` to the end of any `output`,
e.g. `outputs$text_out`.

If you use a dynamic UI and want to reassert the names are still unique,
it is perfectly acceptable to call `AppDriver$expect_unique_names()`
after setting any dynamic UI values.

### Shiny values:

`AppDriver$expect_values()` is the preferred method for testing in
[shinytest2](https://rstudio.github.io/shinytest2/). This method tests
different `input`, `output`, and `export` values provided by the Shiny
application.

- `input` corresponds to the `input` values provided by the Shiny
  application.
- `output` corresponds to the `output` values provided by the Shiny
  application.
- `export` corresponds to value that have been \_export_ed by the Shiny
  application. These values are exported by
  [`shiny::exportTestValues()`](https://shiny.posit.co/r/reference/shiny/latest/exporttestvalues.html)
  from within your `server` function.

When `AppDriver$expect_values()` is called, each `input`, `output`, and
`export` value will be serialized to JSON and saved to a snapshot file
(e.g. `001.json`) for followup expectations.

In addition to this value snapshot file, a *debug* screenshot will be
saved to the snapshot directory with its file name ending in `_.png`
(e.g. `001_.png`). This screenshot is useful for knowing what your
application looked like when the values where captured. However,
differences in the captured screenshot will never cause test failures.
It is recommended to add `_.new.png` to your `.gitignore` file to ignore
any new debug screenshots that have not been accepted.

For typical app testing, this method should cover most of your testing
needs. (Remember, try to only test the content you have control over.)

#### Downloads

When
[`shiny::downloadButton()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)
or
[`shiny::downloadLink()`](https://rdrr.io/pkg/shiny/man/downloadButton.html)
elements are clicked, a file is downloaded. To make an expectation on
the downloaded file, you can use `AppDriver$expect_download()`.

In addition to the file being saved, a snapshot of the file name being
downloaded will be saved if a suggested file name is used.

### UI expectations

Two methods are provided as a middle ground between taking a screenshots
and testing Shiny app values: `AppDriver$expect_text()` and
`AppDriver$expect_html()`.

`AppDriver$expect_text(selector=)` asks the Chrome browser for the
current text contents within the selected elements. This method is great
to test contents that are not `input` or `output` values.
`AppDriver$expect_text()` is not able to retrieve pseudo elements or
values such as the text inside `<text>` or `<textarea>` input elements.

`AppDriver$expect_html(selector=)` asks the Chrome browser for the
current DOM structures within the selected elements. This method is
perfect for app authors who are constructing their own DOM structures
and want to verify that they are consistently being produced. Again,
`AppDriver$expect_html()` is not able to retrieve pseudo elements or
values such as the text inside `<text>` or `<textarea>` input elements.

Finally, both of these methods wrap around
`AppDriver$expect_js(script=)`. This method executes a `script` and the
return value is saved as a snapshot. `AppDriver$expect_text()` and
`AppDriver$expect_html()` are wrappers around `AppDriver$expect_js()`.

### UI visual expectations

Taking a screenshot is the most brittle expectation provided by
[shinytest2](https://rstudio.github.io/shinytest2/).
`AppDriver$expect_screenshot()` should only be used if it is absolutely
necessary or if you are willing to handle many false-positive failures.

There are many ways a screenshot expectation can fail that is unrelated
to your code, including:

- the R version changed
- the operating system changed (e.g. Windows vs. Linux)
- the system font changed
- Chrome is not consistent in how it paints some DOM elements
- the CSS transitions did not finish in time
- a package author (e.g. Shiny,
  [bslib](https://rstudio.github.io/bslib/),
  [htmltools](https://github.com/rstudio/htmltools)) changed its DOM
  structure

App authors who use custom CSS or JavaScript may find this method
useful. But it is still strongly recommended to only test the content
you have control over to avoid false-positive failures.

When expecting a screenshot, a `AppDriver$new(variant=)` must be
supplied. The value may be `NULL`, but it is recommended to use
[`shinytest2::platform_variant()`](https://rstudio.github.io/shinytest2/reference/platform_variant.md).

## Suggested approaches

### Exported values

It cannot be recommended enough to use
[`shiny::exportTestValues()`](https://shiny.posit.co/r/reference/shiny/latest/exporttestvalues.html)
to test your Shiny app’s reactive values.

If we make the assumption that package authors create consistent render
methods, then we can test the values provided to render methods using
[`shiny::exportTestValues()`](https://rdrr.io/pkg/shiny/man/exportTestValues.html).
Let’s look at an example of a Shiny app that displays a plot of the
first `n` rows of data.

It is recommended to make your render methods contain minimal logic so
that the value being rendered can also be exported.

``` r

library(shiny)
library(ggplot2)
ui <- fluidPage(
  numericInput("n", "Number of rows", 10, 1, nrow(cars)),
  plotOutput("plot")
)
server <- function(input, output) {
  dt <- reactive({
    head(cars, input$n)
  })
  plot_obj <- reactive({
    ggplot(dt(), aes_string("speed", "dist")) + geom_point()
  })

  output$plot <- renderPlot({
    plot_obj()
  })

  exportTestValues(
    dt = dt(),
    plot_obj = plot_obj()
  )
}
plot_app <- shinyApp(ui = ui, server = server)
plot_app
```

![Screenshot of a Shiny app displaying a scatter plot with slider
controls, demonstrating how exported values like plot objects and data
can be tested using shinytest2](images/plot-app.png)

Both `dt` and `plot_obj` have been `export`ed. This means that they are
available when executing `AppDriver$get_values()`,
`AppDriver$get_value()`, and `AppDriver$expect_values()`.

``` r

app <- AppDriver$new(plot_app)

values <- app$get_values()
str(values) # Output has been truncated for printing
#> List of 3
#>  $ input :List of 1
#>   ..$ n: int 10
#>  $ output:List of 1
#>   ..$ plot:List of 5
#>   .. ..$ src     : chr "data:image/png;base64,iVBORw0"| __truncated__
#>   .. ..$ width   : int 962
#>   .. ..$ height  : int 400
#>   .. ..$ alt     : chr "Plot object"
#>   .. ..$ coordmap:List of 2 | __truncated__
#>  $ export:List of 2
#>   ..$ dt      :'data.frame': 10 obs. of  2 variables:
#>   .. ..$ speed: num [1:10] 4 4 7 7 8 9 10 10 10 11
#>   .. ..$ dist : num [1:10] 2 10 4 22 16 10 18 26 34 17
#>   ..$ plot_obj:List of 9
#>   .. ..$ data       :'data.frame':   10 obs. of  2 variables:
#>   .. .. ..$ speed: num [1:10] 4 4 7 7 8 9 10 10 10 11
#>   .. .. ..$ dist : num [1:10] 2 10 4 22 16 10 18 26 34 17
#>   .. ..$ layers     :List of 1 | __truncated__
#>   .. ..$ scales     :Classes 'ScalesList', 'ggproto', 'gg' <ggproto object: Class | __truncated__
#>   .. ..$ mapping    :List of 2 | __truncated__
#>   .. ..$ theme      : list() | __truncated__
#>   .. ..$ coordinates:Classes 'CoordCartesian', 'Coord', 'ggproto', 'gg' <ggproto  | __truncated__
#>   .. ..$ facet      :Classes 'FacetNull', 'Facet', 'ggproto', 'gg' <ggproto object: Class  | __truncated__
#>   .. ..$ plot_env   :<environment: 0x7fad13bd3d58>
#>   .. ..$ labels     :List of 2
#>   .. .. ..$ x: chr "speed"
#>   .. .. ..$ y: chr "dist"
#>   .. ..- attr(*, "class")= chr [1:2] "gg" "ggplot"
```

Plots are notoriously hard to test consistently over time and different
platforms. Similar to `AppDriver$expect_screenshot()`, there are many
ways outside of your code that the plot can produce a change which would
fail a visual test. Some of these include:

- Default system font changes, as different operating systems have
  different fonts
- Default system font size changes
- R version changes

When possible, it is recommended to use
[`{ggplot2}`](https://ggplot2.tidyverse.org/) over
[`base::plot()`](https://rdrr.io/r/base/plot.html) as
[ggplot2](https://ggplot2.tidyverse.org) is getting closer and closer to
cross platform compatible. [ggplot2](https://ggplot2.tidyverse.org)
objects can also be inspected and compared using
[`{vdiffr}`](https://vdiffr.r-lib.org/) for cross platform support.

### Snapshots vs values

Snapshot testing makes the test writing minimal and quick. However, this
comes with a logical risk.

Snapshots are wonderful at determining if a change has occurred. But for
the first execution of the snapshot there is nothing to compare, so the
first value is taken as *truth* even if it is not correct. This allows
for the opportunity for invalid values to be saved as *truth*. Most of
the time, it is OK to use snapshots from the beginning of your testing
experience, but keeping track of each snapshot file can be tedious when
testing multiple applications with multiple variants.

To remedy this possibility, app values can be tested against known
static values. For example, instead of testing the initial value of
[`dt()`](https://rdrr.io/r/stats/TDist.html) against a snapshot, we can
test it against `head(cars, 10)`.

``` r

# Snapshot code
app$expect_values(export = "dt")

# Manual expectation code
expect_equal(
  app$get_value(export = "dt"),
  head(cars, 10)
)
```

If you have the time to manually inspect the snapshot files afterwards,
using `AppDriver` snapshot code provides clean, minimal code. If
snapshot files are unwieldy, you can use `AppDriver` to retrieve values
to test against known values.

### Example

Let’s look at how we could write a
[shinytest2](https://rstudio.github.io/shinytest2/) test to make sure
the plot is updated after updating our numeric `input` `n`. Assuming the
Shiny app code above is saved in `app.R`, we can look at
`tests/testthat/test-export.R` below. To successfully run the code
below, be sure to have [vdiffr](https://vdiffr.r-lib.org/) installed.

Since we are not calling `AppDriver$expect_screenshot()` or
`AppDriver$get_screenshot()`, we do not need to use
`AppDriver$new(variant=)`.

``` r

# File: ./tests/testthat/test-export.R
# App file: ./app.R
library(shinytest2)

test_that("`export`ed `plot_obj` is updated by `n`", {
  skip_if_not_installed("vdiffr")

  app <- AppDriver$new()

  # Verify `dt()` uses first 10 lines of `cars`
  n10 <- app$get_value(input = "n")
  expect_equal(n10, 10)
  # Verify `dt10()` data is first 10 lines of `cars`
  dt10 <- app$get_value(export = "dt")
  expect_equal(dt10, head(cars, n10))

  # Verify `plot_obj()` data is `dt()`
  plot_obj_10 <- app$get_value(export = "plot_obj")
  expect_equal(plot_obj_10$data, dt10)
  # Verify `plot_obj()` is consistent
  vdiffr::expect_doppelganger("cars-points-10", plot_obj_10)

  ## Update `n` to 20
  app$set_inputs(n = 20)

  # Verify `n` was updated
  n20 <- app$get_value(input = "n")
  expect_equal(n20, 20)
  # Verify `dt()` uses first 20 lines of `cars`
  dt20 <- app$get_value(export = "dt")
  expect_equal(dt20, head(cars, n20))

  # Verify `plot_obj()` data is `dt()`
  plot_obj_20 <- app$get_value(export = "plot_obj")
  expect_equal(plot_obj_20$data, dt20)
  vdiffr::expect_doppelganger("cars-points-20", plot_obj_20)
})
```

The second half of the test performs similar expectations to the first
half of the test. This code can be pulled into a helper function and be
written as:

``` r

# File: tests/testthat/test-export.R
library(shinytest2)

test_that("`export`ed `plot_obj` is updated by `n`", {
  skip_if_not_installed("vdiffr")

  app <- AppDriver$new(variant = platform_variant())

  expect_n_and_plot <- function(n) {
    # Verify `n` input equals `n`
    n_val <- app$get_value(input = "n")
    expect_equal(n_val, n, expected.label = n)
    # Verify `dt()` data is first `n` lines of `cars`
    dt <- app$get_value(export = "dt")
    expect_equal(dt, head(cars, n), expected.label = paste0("head(cars, ", n, ")"))

    # Verify `plot_obj()` data is `dt()`
    plot_obj <- app$get_value(export = "plot_obj")
    expect_equal(plot_obj$data, dt, info = paste0("n = ", n))
    # Verify `plot_obj()` is consistent
    vdiffr::expect_doppelganger(paste0("cars-points-", n), plot_obj)
  }

  expect_n_and_plot(10)

  # Update `n` to 20
  app$set_inputs(n = 20)
  expect_n_and_plot(20)
})
```

It should be noted that [ggplot2](https://ggplot2.tidyverse.org) objects
can not be serialized to `JSON` and should be excluded from the
`AppDriver$expect_values()` snapshots.

## Cliffs Notes

##### Retrieving values

- `AppDriver$get_values()`, `AppDriver$get_value()`:
  - **Best way to retrieve values** as they have been serialized by
    [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)
  - **Safest way to compare values** as comparisons
    (e.g. [`expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html))
    will need to be explicitly stated from the beginning
  - Pro: Supports more complex objects like
    [ggplot2](https://ggplot2.tidyverse.org) plots
  - Con: Causes code to be more verbose

##### Expectation methods

All `AppDriver$expect_*()` should have their snapshots manually
inspected when first created to ensure they contain expected content.

- `AppDriver$expect_values()`:
  - **Go-to expectation method** for Shiny `input`, `output`, and
    `export` values
  - **Automatically recorded** in `shinytest2::record_app()`
  - Saves a *debug* screenshot for manual inspection
  - Pro: Tests (almost) all Shiny values
  - Con: Complex objects containing environments
    (e.g. [ggplot2](https://ggplot2.tidyverse.org) plots) cannot be
    serialized to JSON
  - Get: `AppDriver$get_value()` and `AppDriver$get_values()`
- `AppDriver$expect_download()`:
  - **Automatically recorded** in `shinytest2::record_app()`
  - Pro: Will save a snapshot of the requested `filename`
  - Get: `AppDriver$get_download()`
- `AppDriver$expect_text()`:
  - Safest way to retrieve text for a given `selector`
  - More stable than than `AppDriver$expect_html()`, as it gives more
    freedom to UI authors to make HTML structure changes
  - Pro: Can test non-Shiny content using the `selector`
  - Con: Cannot be used to test text inputs
  - Get: `AppDriver$get_text()`
- `AppDriver$expect_html()`
  - Expects all HTML to be consistently shaped for a given `selector`
  - More stable than than `AppDriver$expect_screenshot()`, and gives
    more freedom to UI authors
  - Pro: Can test non-Shiny content using the `selector`
  - Con: Cannot be used to test text inputs
  - Get: `AppDriver$get_html()`
- `AppDriver$expect_js()`:
  - Makes an expectation on the JavaScript result
  - Building block for `AppDriver$expect_text()` and
    `AppDriver$expect_html()`
  - Con: Must write own JavaScript code to test
  - Get: `AppDriver$get_js()`
- `AppDriver$expect_screenshot()`:
  - Saves a screenshot of a selector (defaults to `"html"`) for later
    comparison
  - **Strongly recommended to use other testing methods when possible**
  - Pro: A picture can be worth a thousand \[tests\]
  - Con: Screenshot reproducibility is **very** brittle to non-Shiny
    changes
  - Get: `AppDriver$get_screenshot()`
