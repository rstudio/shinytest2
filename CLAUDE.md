# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Overview

shinytest2 is an R package that provides automated testing for Shiny
applications using a headless Chromium browser via the chromote package.
It integrates with testthat for snapshot-based testing of Shiny apps.

## Core Architecture

### Main Components

- **AppDriver (R6 class)**: The central testing interface defined in
  [R/app-driver.R](https://rstudio.github.io/shinytest2/R/app-driver.R).
  This R6 class manages:

  - Launching Shiny apps in background R processes (via callr)
  - Controlling a headless Chrome browser (via chromote)
  - Capturing app state (inputs, outputs, exports)
  - Taking screenshots and executing JavaScript
  - Waiting for reactivity to stabilize

- **Test Recording**:
  [R/record-test.R](https://rstudio.github.io/shinytest2/R/record-test.R)
  implements an interactive test recorder that launches a special Shiny
  app to record user interactions and generate test code automatically.

- **Expectations**: The package provides several expectation methods
  split across files like
  [R/app-driver-expect-values.R](https://rstudio.github.io/shinytest2/R/app-driver-expect-values.R),
  [R/app-driver-expect-screenshot.R](https://rstudio.github.io/shinytest2/R/app-driver-expect-screenshot.R),
  etc. These wrap testthat’s snapshot functions.

### Key Design Patterns

1.  **Split R6 Methods**: The AppDriver R6 class is split across
    multiple files (app-driver-\*.R) for maintainability. Each file
    handles a specific aspect (expectations, waiting, JavaScript
    execution, etc.).

2.  **Background Processes**: Shiny apps run in separate R processes via
    callr to isolate them from the testing environment.

3.  **Chromote Integration**: The package heavily uses chromote to
    control a headless Chrome browser for realistic user interaction
    simulation.

4.  **Snapshot Testing**: Integrates with testthat’s snapshot testing
    infrastructure. Snapshots are stored in tests/testthat/\_snaps/.

5.  **Test Mode**: Shiny apps must run with `test.mode = TRUE` to expose
    internal values via
    [`shiny::exportTestValues()`](https://rdrr.io/pkg/shiny/man/exportTestValues.html).

## Common Development Tasks

### Running Tests

``` r

# Run all package tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-app-driver.R")

# Run tests with filter
devtools::test(filter = "screenshot")
```

### Building the Package

``` r

# Load package for development
devtools::load_all()

# Check package
devtools::check()

# Build documentation
devtools::document()

# Build website
pkgdown::build_site()
```

### Testing During Development

``` r

# Create an AppDriver for interactive testing
library(shinytest2)
app <- AppDriver$new("path/to/app")
app$view()  # Opens browser for visual inspection
app$get_values()
app$set_inputs(input_name = value)
app$stop()
```

### Working with C++ Code

The package includes C++ code (in src/) compiled via cpp11. After
modifying C++ code:

``` r

# Rebuild and reload package
devtools::load_all()
```

## Important Implementation Details

### AppDriver Lifecycle

1.  `$new()` initializes the driver:
    - Starts background R process with Shiny app
    - Launches ChromoteSession
    - Injects shiny-tracer.js for monitoring
    - Optionally waits for app to stabilize
2.  `$stop()` cleans up:
    - Stops Shiny process (SIGINT → SIGTERM → SIGKILL)
    - Closes ChromoteSession
    - Cleans up logs if requested

### Waiting Mechanisms

- `$wait_for_idle()`: Waits for Shiny to be idle (no reactivity) for a
  specified duration
- `$wait_for_value()`: Waits for a specific input/output/export to have
  a non-ignored value
- `$wait_for_js()`: Waits for JavaScript expression to evaluate to true
- All set_inputs() calls wait by default until an output updates

### Screenshot Comparison

- Default selector changed in v0.3.0 from `"html"` to
  `"scrollable_area"`
- [`compare_screenshot_threshold()`](https://rstudio.github.io/shinytest2/reference/compare_screenshot_threshold.md)
  in
  [R/compare-screenshot-threshold.R](https://rstudio.github.io/shinytest2/R/compare-screenshot-threshold.R)
  provides threshold-based comparison using convolution to handle minor
  pixel differences
- Screenshots are platform-dependent; use `variant = platform_variant()`
  for cross-platform testing

### Test File Organization

- Package follows standard R package structure with tests in
  tests/testthat/
- Recorded tests typically saved as test-app-.R
- Support files (helper functions, data) should go in tests/testthat/
  and are loaded via
  [`load_app_support()`](https://rstudio.github.io/shinytest2/reference/app_support.md)

## Migration from shinytest

This package replaces the deprecated shinytest package. Key function:
[`migrate_from_shinytest()`](https://rstudio.github.io/shinytest2/reference/migrate_from_shinytest.md)
in [R/migrate.R](https://rstudio.github.io/shinytest2/R/migrate.R)
converts old shinytest tests to shinytest2 format.

## Logging and Debugging

- `$get_logs()` returns a data.frame with timestamped logs from three
  sources:
  - “shinytest2”: AppDriver logging via `$log_message()`
  - “shiny”: stdout/stderr from the background Shiny process
  - “chromote”: Browser console, exceptions, and WebSocket traffic (if
    `options(shiny.trace = TRUE)`)
- Set `options(shiny.trace = TRUE)` in the app’s options to capture all
  WebSocket messages

## CI/CD

The package uses rstudio/shiny-workflows GitHub Actions: - R-CMD-check
runs on multiple platforms - Website deployment via pkgdown - Test apps
can be tested in CI using the custom action at actions/test-app

## Testing Philosophy

- Prefer `$expect_values()` over `$expect_screenshot()` for robustness
- Prefer `$expect_text()` over `$expect_html()` to avoid brittleness
  from internal DOM changes
- Use `$expect_screenshot()` sparingly as screenshots are very brittle
  to:
  - R version changes
  - OS differences
  - Font differences
  - Package version updates

## Package Dependencies

Critical dependencies to be aware of: - **chromote** (≥ 0.5.0): Browser
automation - **testthat** (≥ 3.3.1): Testing framework - **callr**:
Background R processes - **shiny**: The framework being tested -
**cpp11**: C++ integration for performance-critical operations
