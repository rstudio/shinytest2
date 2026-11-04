---
title: Migrating shinytest tests (migrate_from_shinytest)
description: Read when working on migrate_from_shinytest() in R/migrate.R, the ShinyDriver to AppDriver code rewrite, snapshot file renaming, or the tests/testthat/test-migration-*.R tests.
---

# Migrating shinytest tests

`migrate_from_shinytest(app_dir, ..., clean = TRUE, include_expect_screenshot, quiet = FALSE)` in `R/migrate.R` turns an app's old shinytest tests into shinytest2 + testthat 3 tests. It needs `shinytest` (>= 1.5.1) installed, because it uses shinytest's function signatures to match arguments.

User-facing method mapping lives in `vignettes/z-migration.Rmd`.

## Expected input layout

```
app/
├── app.R
└── tests/
    ├── shinytest.R                # calls shinytest::testApp("..")
    └── shinytest/
        ├── mytest.R               # uses app <- ShinyDriver$new("../../")
        ├── mytest-expected[-SUFFIX]/001.json, 001.png, ...
        └── other files / folders
```

`m__validate_shinytest_exists()` aborts if `tests/`, exactly one `tests/shinytest.R` (case-insensitive `.r`), or `tests/shinytest/` is missing.

## Steps

All steps run with the working directory set to `app_dir`. Shared state is kept in one environment (`app_info_env`) instead of passing return values up and down.

1. **`m__extract_runner_info()`**: parses `tests/shinytest.R` and finds the first `testApp()` call (`m__find_shinytest_testapp()`). If none, it uses `testApp()` defaults with `appDir = ".."`. Aborts if `appDir` is not `".."`. Stores `testnames` (via `shinytest___find_tests()`, a copy of shinytest's logic), `suffix`, and `compareImages`.
2. **`m__write_shinytest2_runner()`**: overwrites `tests/testthat.R` with the shinytest2 runner (`use_shinytest2_runner(overwrite = TRUE)`).
3. **`m__parse_test_files()`**: for each test script:
   - `m__reset_info_env()` clears per-file state (`take_screenshot` is kept so a menu answer is reused).
   - `m__parse_test_file()` rewrites `tests/shinytest/<name>.R` to `tests/testthat/test-<name>.R`, wrapped by `m__testthat_chunk()` in `library(shinytest2)` + `test_that("Migrated shinytest test: <name>.R", {...})`. Files with no code are copied as-is.
   - `m__expected_files()` copies snapshots (see below).
   - Then every other file in `tests/shinytest/` (not `.R`, not `-expected` folders) is copied into `tests/testthat/`.
4. **`m__remove_shinytest_files()`** (only if `clean = TRUE`): deletes `tests/shinytest.R` and `tests/shinytest/`.

## Code rewrite

`m__parse_test_text()`:

1. Finds `<var> <- ShinyDriver$new(...)` assignments (`m__find_shinydriver_new()`). Zero or more than one: abort. Remembers the variable name as `app_var`.
2. Pass 1: `m__algo_2(text, m__shinytest_lang)` rewrites every `<app_var>$method(...)` call.
3. Pass 2: `m__algo_2(text, m__driver_new)` rewrites `ShinyDriver$new()` to `AppDriver$new()`.

### `m__algo_2()`: line-based rewrite

Goes through the text line by line. Comment and blank lines are kept. Otherwise `parse_next_expr()` adds lines until they parse, the expression is walked with `expr_recurse()` (`R/expr-recurse.R`), and if a match was found (`info_env$match_found`) the new deparsed code replaces those lines. If no match, the original lines are kept as they were. Comments inside a changed expression are lost.

### `m__driver_new()`

- `path` is kept unless it is `"../.."` (the default app location).
- `variant` is set to the runner's `suffix` (or `NULL`).
- Renamed args: `loadTimeout` to `load_timeout`, `checkNames` to `check_names`, `seed`, `cleanLogs` to `clean_logs`, `shinyOptions` to `shiny_args`, `renderArgs` to `render_args`, `options`.
- `debug` and `phantomTimeout` are dropped with a message.

### `match_shinytest_expr()`: method mapping

Args are matched against `shinytest::ShinyDriver$public_methods[[method]]`. Main cases:

| shinytest | shinytest2 |
|---|---|
| `setInputs(...)` | `set_inputs(...)`; `allowInputNoBinding_` renamed; `values_` removed |
| `click(name, iotype)` | `click(input =)` / `click(output =)`; `auto` becomes `click(selector = "#name")` |
| `executeScript(script, ...)` | `get_js(script, timeout = 10000)`; extra args become `let arguments_ = <json>;` and `arguments` is renamed |
| `expectUpdate(output, ..., iotype = "input")` | a `local({...})` block: get output, `set_inputs()`, get output, `expect_failure(expect_equal())` |
| `getAllValues()` | `get_values()` |
| `getValue(name, iotype)` | `get_value(input/output =)`; `auto` guesses `output` |
| `setValue(name, value, iotype)` | `set_inputs()`; `auto` guesses `input`; `output` aborts |
| `snapshot(items, screenshot)` | `expect_values(...)`, plus `expect_screenshot()` if screenshots are wanted |
| `snapshotDownload(id, filename)` | `expect_download(id, name =)` |
| `takeScreenshot(file, id)` | `get_screenshot(file, selector = "#id")` |
| `uploadFile(...)` | `upload_file(...)` |
| `waitFor(expr, timeout)` | `wait_for_js(expr, timeout =)` |
| `waitForShiny()` | `wait_for_idle(duration = 0)` |
| `waitForValue(name, iotype, ...)` | `wait_for_value(input/output/export =, ignore, timeout, interval)` |
| `getSource()`, `getTitle()`, `getUrl()`, `getAppDir()` | `get_html("html", outer_html = TRUE)`, `get_js("window.document.title;")`, `get_url()`, `get_dir()` |
| `getWindowSize()`, `setWindowSize()` | `get_window_size()`, `set_window_size()` |
| `goBack()`, `refresh()` | `run_js("window.history.back();")`, `run_js("window.location.reload();")` |
| `logEvent()` | `log_message()` |
| `getDebugLog()`, `getEventLog()` | `get_logs()` (with a message) |
| `isRmd()`, `getAppFilename()` | inline code that checks `get_dir()` for `.Rmd` files |
| `stop()` | `stop()` |
| `snapshotInit(path, screenshot)` | removed; `screenshot` is remembered |
| `enableDebugLogMessages()` | removed |

### Should `snapshot()` also take a screenshot?

Checked in this order (first hit wins):

1. `include_expect_screenshot` argument.
2. Earlier menu answer in this run.
3. `FALSE` if `testApp(compareImages = FALSE)`.
4. `snapshot(screenshot =)`.
5. `snapshotInit(screenshot =)`.
6. Interactive menu (default: no screenshot). Not interactive: abort and ask for `include_expect_screenshot`.

`snapshot(items =)` is converted only if it is missing, `NULL`, or a literal `list(input =, output =, export =)`.

## Snapshot files

`m__expected_files()` copies `tests/shinytest/NAME-expected[-SUFFIX]/` to `tests/testthat/_snaps/[SUFFIX/]NAME/`. File numbers are changed because shinytest numbered a json + png pair with the same number, while shinytest2 counts each expectation:

- `NNN.json` becomes `2*NNN - 1 - downloads_so_far`
- `NNN.png` becomes `2*NNN - downloads_so_far`
- `NNN.download` becomes `2*NNN - 1 - downloads_so_far`, then the download count goes up

## Not supported (aborts with a message)

- `testApp(appDir =)` other than `".."`; `testnames` that are not plain values.
- More than one `ShinyDriver$new()` per test file, or none.
- `executeScriptAsync()`, `findElement()`, `findElements()`, `findWidget()`: the vignette shows manual rewrites.
- `getSnapshotDir()`, `getRelativePathToApp()`, `getTestsDir()`.
- `clone()`, `sendKeys()`.
- `expectUpdate(iotype = "output")`, `setValue(iotype = "output")`.
- Using the return value of `setInputs()` / `uploadFile()` (`values_ = TRUE` or not at top level).
- `takeScreenshot(parent =)` when not `FALSE`.
- Arguments like `iotype` or `snapshotInit(path)` given as variables instead of literal strings.
- Any unknown method name.
- Code that does not call methods on the `ShinyDriver` variable directly (for example, the driver passed into a helper function) is left unchanged.

## Tests

- `tests/testthat/helper-migration-env.R`: `make_info_env()` builds a fake `info_env` for unit tests.
- `tests/testthat/test-migration-transformation.R`: one shinytest expression in, expected shinytest2 expression (and message) out.
- `tests/testthat/test-migration-file.R`: whole-file text through `m__parse_test_text()`.
- `tests/testthat/test-migration-migrate.R`: copies each `tests/testthat/migrate-apps/NN/` to a temp folder, runs `migrate_from_shinytest()`, and compares every file to `migrate-apps/NNex/` (PNGs byte-for-byte). Skipped on CRAN.
- All migration tests need `shinytest` installed. `migrate-apps/` is in `.Rbuildignore`, so these are skipped in the built package.

`migrate_shinycoreci_shinytest()` at the bottom of the file is an internal helper that rewrites shinycoreci's runner file to a standard `testApp()` call before migration.
