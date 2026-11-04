---
title: shinytest2 project brief
description: Start here. What shinytest2 is, its public API, dependencies, folder layout, and a map to the other memory-bank docs.
---

# shinytest2 project brief

## What it is

shinytest2 is an R package for automated testing of Shiny apps. It starts the app in a background R process, opens it in a headless Chrome browser (through the chromote package), drives it like a user would, and records results as testthat snapshots.

It replaces the older `shinytest` package, which used PhantomJS.

## Who uses it

- Shiny app authors who want regression tests for an app folder.
- R package authors who ship Shiny apps (or app functions) inside a package.
- CI users who test apps on GitHub Actions (see `actions/test-app`).

## Main user-facing API

All exports are listed in `NAMESPACE`.

| Export | File | Purpose |
|---|---|---|
| `AppDriver` | `R/app-driver.R` (+ `R/app-driver-*.R`) | R6 class. Starts an app, drives the browser, reads values, takes snapshots. See `appDriver.md`. |
| `test_app()` | `R/test-app.R` | Wraps `testthat::test_dir()` to run an app's `tests/testthat/` folder. |
| `load_app_env()`, `load_app_support()`, `local_app_support()`, `with_app_support()` | `R/test-app.R` | Source an app's `R/` files and `global.R` into an environment so tests can use app objects. |
| `record_test()` | `R/record-test.R` | Launches a recorder app (`inst/internal/recorder/`) that watches user actions and writes test code. |
| `register_input_processor()`, `get_input_processors()` | `R/record-test.R` | Custom input handling for the recorder. |
| `use_shinytest2()`, `use_shinytest2_test()` | `R/use.R` | usethis-style setup: test runner, setup file, ignore entries, `Suggests` entry, and a starter test file (templates in `inst/internal/template/`). |
| `migrate_from_shinytest()` | `R/migrate.R` | Converts old `shinytest` tests to `shinytest2` code. |
| `compare_screenshot_threshold()`, `screenshot_max_difference()` | `R/compare-screenshot-threshold.R` | Screenshot compare that allows small pixel differences. Uses a C++ convolution (`src/code.cpp`, `image_diff_convolution_max_value`). |
| `platform_variant()` | `R/platform.R` | Builds a snapshot `variant` string from OS name and R version. |

## Key dependencies

From `DESCRIPTION`:

- Depends: `testthat (>= 3.3.1)` (edition 3).
- Imports, most important:
  - `chromote (>= 0.5.0)`: headless Chrome control.
  - `callr`: runs the Shiny app in a background R process (`callr::r_bg()`).
  - `R6`: the `AppDriver` class.
  - `shiny`, `rmarkdown`: running apps and Shiny R Markdown / Quarto docs.
  - `httr2`, `pingr`: URL parsing, HTTP requests to Shiny's test API, port checks.
  - `pkgload`: loads a local dev package inside the background app process.
  - `checkmate`, `rlang`, `cli`, `lifecycle`, `withr`, `fs`, `jsonlite`, `globals`.
- LinkingTo: `cpp11` (C++ code in `src/`).
- Suggests worth knowing: `shinytest` (for migration), `png`, `diffobj`, `vdiffr`, `rstudioapi`, `usethis`.

## Package layout

- `R/`: all R code.
  - `app-driver.R` defines the `AppDriver` class and its roxygen docs. Each public method is a thin wrapper that calls an `app_*()` function in a topic file such as `app-driver-wait.R` or `app-driver-expect-values.R`.
  - `chromote-methods.R`: small wrappers around chromote calls (`chromote_eval()`, `chromote_wait_for_condition()`, DOM helpers).
  - `test-app.R`, `use.R`, `record-test.R`, `migrate.R`, `compare-screenshot-threshold.R`, `platform.R`, `save-app.R`, `expect-snapshot.R`: the non-AppDriver features.
  - `DESCRIPTION` has a `Collate:` field. New R files must be added there.
- `src/`: C++ image diff code (`code.cpp`) and cpp11 glue (`cpp11.cpp`, matched by `R/cpp11.R`).
- `inst/internal/js/shiny-tracer.js`: JavaScript injected into every app page. See `waitingAndTimeouts.md`.
- `inst/internal/recorder/`: the recorder Shiny app (`app.R`, `recorder.js`).
- `inst/internal/template/`: files copied by `use_shinytest2()`.
- `inst/gha/`: example GitHub Actions workflow and script for app audits.
- `inst/example/`, `inst/vig-apps/`: images and apps used by docs and vignettes.
- `tests/testthat/`: package tests. Test apps live in `apps/`, `local-apps/`, `migrate-apps/`, `pkgs/`. Snapshots in `_snaps/`.
- `vignettes/`: user guides (getting started, robust testing, packages, CI, migration, FAQ, monkey testing, app audits).
- `actions/test-app/`: a composite GitHub Action (`action.yml`) that tests one or more app folders and can upload snapshots.

## Memory-bank map

- `appDriver.md`: how `AppDriver` is built, started, and stopped; private state; local vs URL modes.
- `waitingAndTimeouts.md`: `wait_for_idle()`, `wait_for_value()`, `wait_for_js()`, how `set_inputs()` waits, `shiny-tracer.js`, and timeout resolution.
- `snapshotExpectations.md`: `expect_values()`, `expect_screenshot()`, `expect_text()`, `expect_download()`, and the testthat snapshot glue.
- `loggingAndDebugging.md`: `get_logs()`, `log_message()`, chromote console capture, `view()`.
- `testRunnerAndSetup.md`: `test_app()`, `load_app_env()` and friends, `use_shinytest2()`.
- `testRecorder.md`: `record_test()` and the recorder app.
- `migrateFromShinytest.md`: `migrate_from_shinytest()` code rewriting.
- `ciAndTesting.md`: the package's own test suite, CI workflows, and the `test-app` action.
