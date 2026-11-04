---
title: Package test suite and CI
description: Read when adding or fixing tests in tests/testthat/, updating snapshots, debugging CI failures (R-CMD-check, test-actions), or changing the reusable actions/test-app GitHub action or inst/gha files.
---

# Package test suite and CI

How shinytest2 tests itself, and the GitHub Actions around it.

## Running tests

```r
devtools::test()                              # all tests
devtools::test(filter = "app-image")          # one file: test-app-image.R
testthat::test_file("tests/testthat/test-app-hello.R")
testthat::snapshot_review()                   # look at changed snapshots
devtools::check()                             # full R CMD check
```

Most tests start a real Shiny app and a headless Chrome through chromote, so Chrome must be installed. `tests/testthat.R` is the standard `test_check("shinytest2")`.

## Layout of `tests/testthat/`

| Path | What it holds |
|---|---|
| `test-app-*.R` | `AppDriver` feature tests. Many define the app inline (`shinyApp(ui, server)`), others point at `apps/` |
| `apps/` | Test apps used with `AppDriver$new(test_path("apps/<name>"))`: `hello`, `wait`, `task-button`, `rmd-shiny`, `rmd-pre`, `qmd`, `dir-renviron`, `dir-rprofile`, `files-*`, `upload-multi`, `widgets`, etc. Some have their own `tests/testthat/` |
| `local-apps/` | Full standalone apps with their own `tests/testthat.R` and tests (`html-text`, `window`). Run by `test-local-apps.R` through `test_app(..., quiet = TRUE)` |
| `pkgs/` | Small packages: `expkg`, `golem`, `rhino`. Run by `test-pkgs.R` through `test_app(test_path("pkgs/expkg"), load_package = "source", quiet = TRUE)`. golem and rhino tests are skipped if those packages are missing |
| `migrate-apps/` | `NN/` (shinytest input) and `NNex/` (expected output) pairs for `test-migration-migrate.R` |
| `app-files/` | Data files used by tests (`bear.png`, `cars.csv`) |
| `scripts/` | R scripts run with `callr::rscript()` by `test-save-app.R` (issue repros for app objects and globals) |
| `_snaps/` | testthat snapshots. Subfolders per test file, plus variant folders like `_snaps/mac/` |
| `test-migration-*.R` | Migration tests, see `memory-bank/migrateFromShinytest.md` |
| `test-use.R`, `test-test-app*.R`, `test-pkg.R`, `test-record-test.R` | Setup helpers, `test_app()`, dev package detection, recorder file names |

`test_app()` inside a package test is deprecated for users, but this suite still uses it (with `quiet = TRUE`) to run `local-apps/` and `pkgs/` tests as part of the main run.

### Helpers and setup files

- `helper-skip-if.R`:
  - `skip_if_no_apps()`: skip if `apps/hello` is missing.
  - `skip_if_no_local_apps()`: skip if `local-apps/window` is missing.
  - `skip_if_chrome_older_than(app, major)`: reads the Chrome major version from `app$get_chromote_session()$Browser$getVersion()` and skips if older.
- `helper-migration-env.R`: loads shiny and defines `make_info_env()` for migration unit tests.
- `setup-ci-timeouts.R`: on CI (`CI` env var true), sets `SHINYTEST2_LOAD_TIMEOUT` to 60 s and `SHINYTEST2_TIMEOUT` to 20 s unless already set. Env vars (not options) so child processes from `callr::rscript()` also get them.
- `setup-disable-crashpad.R`: on CRAN, adds `--disable-crash-reporter` to Chrome args and deletes `tempdir()/Crashpad` at the end, to avoid a CRAN NOTE about leftover files.

## Why folders go missing: `.Rbuildignore`

`tests/testthat/apps`, `local-apps`, `pkgs`, and `migrate-apps` are in `.Rbuildignore`. In a built tarball (R CMD check, CRAN) they do not exist, so the `skip_if_no_*()` helpers and inline checks in `test-pkgs.R`, `test-pkg.R`, and `test-migration-migrate.R` skip those tests. Tests that must run there use inline apps.

## CRAN and CI behavior

- `AppDriver$new()` calls `testthat::skip_on_cran()` while testing, unless env var `SHINYTEST2_APP_DRIVER_TEST_ON_CRAN` is `"true"` or `"1"` (`R/app-driver-initialize.R`). CRAN asked that Chrome not be used in CRAN checks.
- Also while testing, it tries to start a chromote session and skips the test if Chrome cannot start. On Windows CI it tries once first to "kick start" chromote.
- `on_cran()` is `NOT_CRAN != "true"`; `on_ci()` is `as.logical(CI)` (`R/utils.R`).
- Under `testthat::is_checking()`, the dev package is not loaded from source into the app process (`R/app-driver-start.R`).
- `test-test-app-reporter.R` checks the testthat reporter API that `test_app()` relies on. It is meant to run on CRAN.
- `test-save-app.R` runs only on CI and not on Windows.

## Snapshots and variants

- `AppDriver$new(variant =)` picks the snapshot subfolder. `platform_variant()` (`R/platform.R`) gives `"<os>-<R major.minor>"`, with `os` one of `mac`, `linux`, `windows`, `solaris`. `platform_variant(r_version = FALSE)` gives only the OS (used in `test-app-hello.R`, stored in `_snaps/mac/`).
- `variant = NULL` stores snapshots in the plain `_snaps/<file>/` folder. `test-app-image.R` uses this with a CSS selector and `threshold =` so one snapshot works on all OSes.
- `$expect_screenshot()` errors if no variant was given at all (`R/app-driver-expect-screenshot.R`). `test-app-hello-variant.R` checks this.
- `_snaps/mac-4.1/` holds old snapshots from an R 4.1 variant.
- `*_.new.png` files are debug images and are git-ignored.

## Screenshot gotchas

- Screenshots differ by OS, fonts, R version, and Chrome version. Prefer `$expect_values()` and `$expect_text()` in new tests.
- chromote PNG bytes can differ between environments even when pixels match (zlib). Use `expect_screenshot(threshold =)` (needs `png`, a Suggests package; tests call `skip_if_not_installed("png")`).
- Chrome before 156 crops the 100px `#green` box in `test-app-image.R` to 99px. Those tests call `skip_if_chrome_older_than(app, 156)` (commit "Skip #green screenshot tests on Chrome < 156").
- Several tests use `skip_on_os("windows")` because downloads, logs, timeouts, and screenshot sizes differ there.

## GitHub workflows (`.github/workflows/`)

### `R-CMD-check.yaml` ("Package checks")

Runs on pushes to `main` and `rc-**`, PRs to `main`, and weekly (Monday). Calls reusable workflows from `rstudio/shiny-workflows@v1`:

- `website.yaml`: pkgdown site.
- `routine.yaml`: routine upkeep. Runs `.github/shiny-workflows/routine.sh`, which re-renders `actions/test-app/README.Rmd` and commits `README.md` if changed.
- `R-CMD-check.yaml`: multi-platform check (`cache-version: "4"`).

`DESCRIPTION` has `Config/Needs/check: rstudio/shiny, bslib`, so checks use dev shiny.

### `test-actions.yaml`

Tests the `test-app` action on ubuntu with R release. Installs deps with `needs: shinytest2-testing` (`Config/Needs/shinytest2-testing: decor`) plus `local::.`. Then:

- Runs the published action `rstudio/shinytest2/actions/test-app@actions/v1` on `tests/testthat/local-apps/window`.
- On PRs, runs the local `./actions/test-app` on several `local-apps/` paths, mixing comma and newline separators to test path parsing.

## Reusable action (`actions/test-app/`)

`action.yml` is a composite action for users' repos:

- Input `app-dir`: one or more app paths, split on commas or whitespace (paths must not contain spaces). Default `"."`.
- Input `upload-snapshots`: default `true`.
- For each app, runs `shinytest2::test_app(app_dir)` inside its own `callr::r()` process so apps do not affect each other. Errors are caught with `try()`; the step fails at the end if any app failed.
- Uploads `<app>/tests/testthat/_snaps` as artifact `<OS>-r<R version>-testthat-snapshots`.
- Example workflows: `example-test-app-description.yaml`, `example-test-app-package.yaml`, `example-test-app-renv.yaml`. Users are pointed to these from `vignettes/use-ci.Rmd`.
- Versioning: a sliding git tag `actions/vX` (and legacy `v1`), separate from package releases. After changing the action, force-move the tag (steps in `actions/test-app/README.Rmd`).
- `actions/` is in `.Rbuildignore`.

## `inst/gha/`

Files for the app audit vignette (`vignettes/use-application-audit.Rmd`): `audit-app.yaml` (a workflow that installs shinycannon, runs a load test, and deploys to GitHub Pages) and `audit-app.R` (helper functions it sources). They are shipped for users to copy; this repo's CI does not run them.

## Invariants / gotchas

- Test apps under `apps/`, `local-apps/`, `pkgs/`, `migrate-apps/` must be guarded by a skip, since they are not in the built package.
- New screenshot snapshots should use a variant or a `threshold`, and expect platform differences.
- When a testthat update breaks `test_app()`, look at `test-test-app-reporter.R` first.
- CI timeouts come from env vars in `setup-ci-timeouts.R`; local runs use the normal defaults.
