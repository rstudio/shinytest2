---
title: Test recorder (record_test)
description: Read when working on record_test(), the recorder Shiny app in inst/internal/recorder/, how browser events become AppDriver test code, where recorded test files are saved, or register_input_processor().
---

# Test recorder (`record_test()`)

`record_test()` opens a "recorder" Shiny app in a browser. The recorder shows the target app in an iframe, watches what the user does, and writes `AppDriver` test code when the user clicks "Save test and exit".

Main files:

- `R/record-test.R`: `record_test()`, `record_test_file_name()`, `register_input_processor()`, `get_input_processors()`
- `R/record-test-unique-name.R`: `known_app_driver_name_values()`
- `R/expr-recurse.R`: `expr_recurse()`, `st2_expr_text()`
- `R/shiny-browser.R`: `with_external_shiny_browser()`
- `inst/internal/recorder/app.R`: the recorder Shiny app (UI, server, code generation, saving)
- `inst/internal/recorder/www/inject-recorder.js`: runs in the recorder page; injects code into the iframe and collects events
- `inst/internal/recorder/recorder.js`: injected into the target app's page; listens to Shiny events
- Tests: `tests/testthat/test-record-test.R` (file name logic only)

## Flow

1. `record_test()` checks args, needs `shinyvalidate` (>= 0.1.2), and rejects `ShinyDriver` objects and `http(s)://` URLs.
2. Package detection: if `record_in_package = TRUE` and `dev_pkg_path()` finds a package in development (installed packages do not count), `package_path` is set. Otherwise `record_in_package` becomes `FALSE`.
3. `test_file` defaults to `record_test_file_name(app, record_in_package)`:
   - In a package and `app` is a path: `test-app-<folder name>.R`.
   - Otherwise: `test-shinytest2.R`.
4. The target app is started as an `AppDriver` (from an app object, a function, or a path). For a path with a Shiny Rmd and no `seed`, a random seed is picked.
5. `name` defaults to the app folder name (not for function apps).
6. `url <- app$get_url()`. In RStudio it is translated with `rstudioapi::translateLocalUrl()`, or rewritten to `../../p/<port>/` on old RStudio Server.
7. Values go to the recorder app through `options()`: `shinytest2.recorder.url`, `shinytest2.app`, `shinytest2.name`, `shinytest2.load.timeout`, `shinytest2.seed`, `shinytest2.shiny.args`, `shinytest2.test_file`, `shinytest2.package_path`, `shinytest2.record_screen_size`, `shinytest2.allow_no_input_binding`.
8. `shiny::runApp(system.file("internal", "recorder", package = "shinytest2"))` runs inside `with_external_shiny_browser()`. That helper opens the recorder in local Chrome (from chromote) when found, or forces RStudio Desktop to use an external browser. It does nothing on RStudio Server.
9. The recorder returns `list(test_file = <path or NULL>)` from `stopApp()`.
10. If a file was saved and `run_test = TRUE`: open it if `open_test_file`, then run it with a filter made from the file name (`test-` prefix and extension removed):
    - Package: `testthat::test_local(package_path, filter =)`
    - Not a package: `test_app(app_dir, filter =)` (or `test_app(".")` for function apps)

## Browser side

Two pages talk with `postMessage`:

- `inject-recorder.js` (recorder page) polls the iframe every 100 ms until jQuery is there, then injects `recorder.js` (sent from the server with the `recorder_js` custom message) and a random token. Messages without the matching token are ignored.
- `recorder.js` (target app page) sends these event types to the parent:
  - `inputEvent` on `shiny:inputchanged`, with `inputType`, `name`, `value`, `hasBinding`, `priority`. It skips values that did not change, inputs changed by `update*Input()` (`shiny:updateinput`), and inputs created by an output render on the same tick.
  - `outputEvent` on `shiny:value` (debounced).
  - `expectDownload` on `shiny:filedownload`.
  - `setWindowSize` on resize and on start.
  - `expectValues` (optionally with one input/output id on Ctrl/Cmd-click), `expectScreenshot`, `waitForIdle` from keyboard shortcuts Ctrl/Cmd-Shift-V, -S, -I.
- The recorder page also adds `expectValues` / `expectScreenshot` when its own buttons are pressed, and filters out `.clientdata` inputs.
- All events go to the server as one growing list: `Shiny.onInputChange("testevents:shinytest2.testevents", ...)`. The `shinytest2.testevents` input handler returns the value as-is to avoid Shiny's unlisting.

## Server side: events to code (`inst/internal/recorder/app.R`)

### `trim_testevents()` reactive

First it removes noise, repeating until nothing changes:

- Back-to-back `outputEvent`, `waitForIdle`, or `setWindowSize`: keep only the last.
- Back-to-back `inputEvent`s that both lack a binding: drop the later one unless unbound inputs are allowed.
- `setWindowSize`, `outputEvent`, `setWindowSize`: drop the first resize.
- `record_screen_size = FALSE`: drop resizes (the check skips the first event).

Then each event gets `app_code`:

| Event | Code |
|---|---|
| `initialize` | none |
| `waitForIdle` | `app$wait_for_idle()` |
| `setWindowSize` | `app$set_window_size(width =, height =)`; the first one is marked `first_set_window_size` |
| `expectDownload` | `app$expect_download("<id>")` |
| `expectScreenshot` | `app$expect_screenshot()` |
| `expectValues` | `app$expect_values()` or `app$expect_values(<type> = "<id>")` |
| `inputEvent`, action button | `app$click("<id>")` |
| `inputEvent`, file upload | `app$upload_file(<id> = "<file>")`, plus a warning comment and `rlang::warn()` code if the file is not under `tests/testthat` |
| `inputEvent`, unbound and not allowed | comment only: "Update unbound `input` value" |
| `inputEvent`, other | `app$set_inputs(<id> = <value>)`, adding `allow_no_input_binding_ = TRUE` and `priority_ = "event"` when needed |
| `outputEvent` | comment only: "Update output value" |

Comments are `st2_comment` objects. They show in the sidebar but are not saved, except the upload warning's `save_code`.

### Input processors

`process_input_value(value, input_type)` turns a browser value into R code text:

- Built in: `default` (strings quoted and escaped, vectors as `c(...)`, empty as `character(0)`), `shiny.action` (becomes a click), `shiny.datetime` (seconds to ms), `shiny.fileupload` (file names).
- Other packages add more with `register_input_processor(input_type, processor)`. The processor takes one arg `value` and returns R code text.
- For an unknown `inputType` like `mypkg.foo`, the recorder tries `requireNamespace("mypkg")` once, then merges processors again. If still unknown it uses `default`.

### `generate_test_code(events, name, seed)`

Builds one `test_that()` block:

```r
test_that("{shinytest2} recording: <name>", {
  local_app_support(test_path("<rel path>"))   # package mode only
  app <- AppDriver$new(
    test_path("<rel path>"),                    # path apps only
    variant = platform_variant(),               # only if a screenshot was taken
    name = "<name>", seed = <seed>,
    height = <h>, width = <w>,                  # from the first resize
    load_timeout = <ms>, shiny_args = list(...)
  )
  <event code>
})
```

The first resize goes into `AppDriver$new()` as `height`/`width` instead of a `set_window_size()` call. The code is parsed and reformatted with `rlang::expr_text(width = 78)`.

## Where the file is saved

Set at the top of `inst/internal/recorder/app.R`:

| App type | `here_dir` | Test file |
|---|---|---|
| Path, not in a package | app folder | `<app>/tests/testthat/<test_file>` |
| Path, in a package | package root | `<pkg>/tests/testthat/<test_file>` |
| Function | `getwd()` | none (saving disabled) |

The app path in the code is `test_path("<app folder relative to tests/testthat>")`.

### On "Save test and exit"

1. Generate code and add it to the end of the test file (`cat(append = TRUE)`). `library(shinytest2)` is added at the top only if the file does not already have it.
2. Standalone apps only: if `<here_dir>/tests/testthat.R` is missing, create it from the template. If it exists but has no `test_app(` call, warn and overwrite it with `shinytest2::test_app()`. Packages keep their own runner (e.g. `testthat::test_check()`).
3. Package mode: `use_shinytest2_package()` (add to Suggests). Otherwise: `use_shinytest2_setup()` (add `load_app_support()` setup file). The generated code's `local_app_support()` line is the package-mode partner of this setup file.
4. Return the file path and close the browser window.

"Exit" returns `test_file = NULL`, so nothing is saved or run.

## Validation (shinyvalidate)

"Save test and exit" stays disabled until all pass:

- At least one `expectValues`, `expectScreenshot`, or `expectDownload` event.
- Test name not already used by an `AppDriver$new(name =)` in the target file. `known_app_driver_name_values()` parses the file and uses `expr_recurse()` to find `AppDriver$new` calls (also with `shinytest2::` / `shinytest2:::`).
- Seed is an integer or empty.
- App is not an app object (its saved copy lives under `tempdir()`).
- App is not a function.

If an unbound input event shows up and `allow_no_input_binding` is `NULL`, a modal asks "Record" or "Ignore" once.

## `expr_recurse()` (`R/expr-recurse.R`)

Walks a parsed expression from leaves up to the root and calls `post_fn(expr_list, is_top_level)` on each call. `post_fn` returns the (maybe changed) call. Missing args are skipped; `NULL` values are kept. Used by `known_app_driver_name_values()` and heavily by `R/migrate.R`. `st2_expr_text()` deparses with width 60 and fixes indents.

## Invariants / gotchas

- App objects and function apps can be recorded, but the test cannot be saved. Users copy the code from the sidebar.
- Remote URLs and `ShinyDriver` objects are not supported.
- Shiny Rmd apps get a random seed if none is given, because their UI has random ids.
- Uploaded files must be under `tests/testthat` (relative to `here_dir`) to avoid the inserted warning.
- The test runner check only runs for standalone apps. In package mode, `tests/testthat.R` is never touched, which matches `use_shinytest2()` defaults (`runner = FALSE` in a package).
- Snapshot names: each `AppDriver` `name` should be unique in a test file, or snapshot files can overwrite each other.
- The recorder echoes the target app's stdout/stderr (from `app$get_logs()`) to the console every 500 ms.
