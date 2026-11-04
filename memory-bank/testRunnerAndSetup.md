---
title: Test runner, project setup, and app support loading
description: Read when working on test_app(), use_shinytest2(), load_app_support()/local_app_support(), dev package detection (R/pkg.R), app objects saved to disk (R/save-app.R), or the recommended test layouts for standalone apps and packages.
---

# Test runner, project setup, and app support loading

This doc covers how users wire shinytest2 into their projects and how the package finds the app, the app's helper files, and the package being developed.

Main files:

- `R/test-app.R`: `test_app()`, `load_app_env()`, `load_app_support()`, `local_app_support()`, `with_app_support()`
- `R/use.R`: `use_shinytest2()`, `use_shinytest2_test()` and internal helpers
- `R/pkg.R`: `dev_pkg_path()`, `dev_pkg_name()`, `dev_pkg_ns()`, `in_dev_pkg()`
- `R/save-app.R`: `app_save()`, `app_data()`, `app_server_globals()`
- `R/app-driver-dir.R`: `app_set_dir()`, `app_dir_value()`, `app_dir_rmd()`
- Templates: `inst/internal/template/testthat.R`, `inst/internal/template/test-shinytest2.R`, `inst/internal/app-template.R`
- Vignettes: `vignettes/use-package.Rmd`, `vignettes/shinytest2.Rmd`, `vignettes/use-ci.Rmd`

## `test_app()`

`test_app(app_dir, ..., name, reporter, stop_on_failure, check_setup, quiet)` is a thin wrapper around `testthat::test_dir()` on `<app_dir>/tests/testthat`. Extra `...` args go to `test_dir()` (for example `filter =` or `load_package =`).

### Finding `app_dir`

- If `app_dir` is missing and the working directory is named `tests` and has a `testthat` folder, it uses `"../"`. This is the case when `tests/testthat.R` calls `shinytest2::test_app()`.
- Otherwise it uses `"."`.
- `app_dir_value()` turns a file path into its parent folder.

### Called outside a testthat test

`stop_on_failure` defaults to `testthat::test_dir()`'s default. Results come back invisibly.

### Called inside a testthat test (deprecated since v0.5.0)

When `testthat::is_testing()` is TRUE:

1. A warning says this use is deprecated. `quiet = TRUE` (or running on CRAN) hides it.
2. It looks for a `SnapshotReporter` in the current reporter (directly, or inside a `MultiReporter`).
3. If found, it ends the current test, context, and file on the outer reporter, and uses `withr::defer()` to restart them when done.
4. It builds a local `ReplayReporter` R6 class that forwards every call to the outer reporter. Its `start_file()` renames the file to `test-<outer file> - <name> - <app test file>` so output shows which app is running. `name` defaults to the app folder name. Its `end_file()` restarts the outer file.
5. `stop_on_failure` defaults to `FALSE` so failures go to the outer reporter instead of stopping.

This relies on testthat reporter internals. `tests/testthat/test-test-app-reporter.R` exists to catch testthat API changes and is meant to run on CRAN.

`check_setup` is deprecated and ignored. Passing `TRUE` gives a lifecycle warning.

## Loading app support files

Shiny apps can have `global.R` and an `R/` folder. These helpers run those files in the test process so tests can use the same objects. All of them call `shiny::loadSupport()`.

| Function | Where objects go | Cleanup |
|---|---|---|
| `load_app_support(app_dir, envir)` | `R/` files into a new child env of `envir`; `global.R` into `envir` | None. Returns the env invisibly |
| `local_app_support(app_dir, envir)` | A new env (parent `globalenv()`) attached with `withr::local_environment()` | When `envir` exits |
| `with_app_support(app_dir, expr)` | Same, via `withr::with_environment()` | After `expr` |
| `load_app_env(app_dir = "../../", renv, globalrenv)` | Superseded, internal. Direct call to `shiny::loadSupport()` | None |

Recommended use (from roxygen docs and `vignettes/use-package.Rmd`):

- Standalone app: `load_app_support()` in `tests/testthat/setup-shinytest2.R`.
- Package: `local_app_support()` or `with_app_support()` inside each `test_that()` so different apps do not leak into each other.
- Inline app objects or functions: not needed. The test already sees package functions.

## `use_shinytest2()`

usethis-style setup. Four flags:

| Flag | Internal helper | What it writes |
|---|---|---|
| `runner` | `use_shinytest2_runner()` | Copies `inst/internal/template/testthat.R` to `tests/testthat.R`. Content: `shinytest2::test_app()` |
| `setup` | `use_shinytest2_setup()` | Adds `shinytest2::load_app_support(test_path("../.."))` to `tests/testthat/setup-shinytest2.R` |
| `ignore` | `use_shinytest2_ignore()` | Adds `*_.new.png` to `.gitignore`; adds `_\.new\.png$` to `.Rbuildignore` only if that file exists |
| `package` | `use_shinytest2_package()` | `usethis::use_package("shinytest2", "Suggests")` if a `DESCRIPTION` exists |

### Default flag rules

- All flags missing and `in_dev_pkg()` is TRUE: `runner = FALSE`, `setup = FALSE`, `ignore = TRUE`, `package = TRUE`. It tells the user to use `local_app_support()` / `with_app_support()`.
- All flags missing and not in a package: all TRUE.
- Any flag given: the missing ones become `FALSE`. This keeps future flags opt-in.
- All `FALSE`: error.

### Helper details

- `use_shinytest2_setup()` skips if `setup.R` or `setup-shinytest2.R` already calls `load_app_env`, or `setup-shinytest2.R` already calls `load_app_support`.
- `use_shinytest2_ignore()` calls `write_union()` directly, not `usethis::use_git_ignore()`, because the usethis helpers keep a sticky project path.
- `copy_test_file_helper()` does not overwrite an existing file unless `overwrite = TRUE`. It says whether the existing file is identical or different.
- `use_shinytest2_test()` copies `inst/internal/template/test-shinytest2.R` (an `AppDriver$new()` + `app$expect_values()` test) to `tests/testthat/test-shinytest2.R`. It also creates the runner if missing.
- The recorder app calls `use_shinytest2_runner()`, `use_shinytest2_setup()`, and `use_shinytest2_package()` on save. See `memory-bank/testRecorder.md`.

## Dev package detection (`R/pkg.R`)

- `dev_pkg_path()` returns `pkgload::pkg_path()` (searches up from the working directory for a `DESCRIPTION`), or `NULL`.
- It returns `NULL` if that folder has `Meta/package.rds`. Only installed packages have `Meta/`. This was added so installed packages (for example tests run with `testthat::test_package()`) are not treated as source packages (commit "Do not treat installed packages as dev packages", `tests/testthat/test-pkg.R`).
- `dev_pkg_name()` reads the name from that path.
- `dev_pkg_ns()` is `pkgload::pkg_ns()`.
- `in_dev_pkg()` is TRUE only when the working directory equals the dev package root.

### How the dev package reaches the background app

In `app_start_shiny()` (`R/app-driver-start.R`):

- `package_path` / `package_name` come from `dev_pkg_path()` / `dev_pkg_name()`.
- Under `testthat::is_checking()` (R CMD check) both are set to `NULL`, so only installed packages are used.
- If set, the background R process replaces `library()` and `require()` in its global env. When the app calls `library(<dev pkg>)` or `require(<dev pkg>)` the first time, it runs `pkgload::load_all(path, export_all = FALSE, export_imports = TRUE, attach_testthat = FALSE)`, then the real function.
- If the app folder is the package root, it sets `options(shiny.autoload.r = FALSE)` so Shiny does not source the package's `R/` folder as app files.

In `app_set_dir()` (`R/app-driver-dir.R`), if `app_dir` is a function whose environment is the dev package namespace, it is wrapped in a new function (env = `globalenv()`) that calls `library(<pkg>)`, gets the namespace, sets the function's env to it, and calls it. So `AppDriver$new(run_app)` works with no wrapper.

## App objects saved to disk (`R/save-app.R`)

`AppDriver$new()` (in `R/app-driver-initialize.R`) calls `app_save()` when given a `shiny.appobj`. `app_save()`:

1. Copies `inst/internal/app-template.R` to `<temp dir>/app.R`.
2. Saves `app_data(app)` to `data.rds`: `ui` (from the `httpHandler` env, with a fallback for `shinyAppDir()` wrappers), `server`, `globals`, `resources` (`shiny::resourcePaths()`), and `packages`.
3. The template loads the packages, re-adds resource paths, puts globals into `globalenv()`, and calls `shinyApp(ui, server)`.

`app_server_globals()` uses the `globals` package (Suggests, >= 0.14.0). First pass uses `mustExist = FALSE`. Second pass uses `mustExist = TRUE` and turns "failed to locate" errors into a warning. Globals that live in a package are dropped and that package is recorded instead.

Limit: only objects named in the server code are found. Objects looked up by name at run time (for example `get(x, globalenv())`) are not carried over. `vignettes/use-package.Rmd` says to pass a function instead.

## `app_dir` rules (`R/app-driver-dir.R`)

- Must be an existing folder (a file path is turned into its folder).
- `app.R` and `server.R` together: error.
- `app.R` with a Shiny Rmd: error. Rmd with `server.R` is allowed; the Rmd wins.
- `app_dir_rmd()` finds `.Rmd`/`.qmd` files not starting with `_` whose front matter `runtime` (or Quarto `server`) starts with `shiny`.

## Recommended layouts (from vignettes)

### Standalone app (`vignettes/shinytest2.Rmd`)

```
app/
├── app.R
├── R/                     # optional helpers
└── tests/
    ├── testthat.R         # shinytest2::test_app()
    └── testthat/
        ├── setup-shinytest2.R   # load_app_support(test_path("../.."))
        ├── test-shinytest2.R
        └── _snaps/
```

Run with `test_app("path/to/app")`.

### Package (`vignettes/use-package.Rmd`, v0.5.0 style)

All app tests live in the package's own `tests/testthat/`. Do not call `test_app()` inside package tests. Three ways to give the app:

1. App folder in `inst/<app>/`: `AppDriver$new(system.file(package = "pkg", "<app>"))`. Use `local_app_support()` if the app has its own `R/` helpers. Private apps can live in `tests/testthat/apps/` and use `test_path()`.
2. A package function such as `run_app`: `AppDriver$new(run_app)`. Or a wrapper `function() { library(pkg); run_app() }`. Objects in the function's env are copied to the background process; the global env is not.
3. An app object: `AppDriver$new(hello_world_app())`. For quick checks only, since not all globals survive saving.

The package must be loaded in the app with `library()` / `require()`, not `pkg::fn()`, for the dev package source to be loaded. The package does not need to be installed for `testthat::test_local()`.

Before v0.5.0, tests lived inside `inst/<app>/tests/`. The vignette shows how to move them to `tests/testthat/` and use `local_app_support()`.

### CI (`vignettes/use-ci.Rmd`)

Covers single-app repos, multi-app repos, and packages, using the `actions/test-app` GitHub action. See `memory-bank/ciAndTesting.md`.

## Invariants / gotchas

- `test_app()` only runs testthat files in `<app_dir>/tests/testthat`. `shiny::runTests()` runs all runners.
- `local_app_support()` / `with_app_support()` load into a fresh env whose parent is `globalenv()`, not the caller env. `load_app_support()` uses the caller env.
- `dev_pkg_path()` must stay `NULL` for installed packages. Without the `Meta/` check, `pkgload::pkg_path()` finds the installed package's `DESCRIPTION` and shinytest2 tries to `load_all()` it.
- `in_dev_pkg()` compares `getwd()` to the package root exactly. A subfolder of a package is not "in" the package for `use_shinytest2()` defaults.
- `record_test()` uses `pkgload::pkg_path()` directly, not `dev_pkg_path()`.
- The replaced `library()` / `require()` show up in `ls(globalenv())` inside the app process.
