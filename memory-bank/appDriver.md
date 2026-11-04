---
title: AppDriver class internals
description: Read when changing AppDriver startup, shutdown, private state, app_dir handling, the background Shiny process, the chromote session, or click/set_inputs/upload/window helpers.
---

# AppDriver class internals

`AppDriver` is the R6 class that runs a Shiny app and drives it in headless Chrome. It is defined in `R/app-driver.R` with `cloneable = FALSE`.

## How the class is split across files

- `R/app-driver.R` holds the class definition, all roxygen docs, the `private` field list, and the public methods.
- Each public method is a one-line wrapper: `foo = function(...) app_foo(self, private, ...)`. The real code lives in topic files:

| File | Functions |
|---|---|
| `app-driver-initialize.R` | `app_initialize()`, `app_initialize_()` |
| `app-driver-start.R` | `app_start_shiny()`, `app_wait_for_serving()` |
| `app-driver-dir.R` | `app_set_dir()`, `app_get_dir()`, `app_dir_rmd()`, `app_dir_has_rmd()`, `app_dir_value()` |
| `app-driver-chromote.R` | `app_get_chromote_session()`, `app_view()` |
| `app-driver-stop.R` | `app_get_process()`, `app_stop()`, `app_finalize()` |
| `app-driver-timeout.R` | timeout resolution, `app_init_timeouts()`, `app_get_timeout()` |
| `app-driver-wait.R` | `app_wait_for_js()`, `app_wait_for_idle()`, `app_wait_for_value()` |
| `app-driver-set-inputs.R` | `app_set_inputs()`, `app_queue_inputs()`, `app_flush_inputs()` |
| `app-driver-node.R` | `app_click()`, `app_find_node_id()`, `node_id_css_selector()` |
| `app-driver-upload-file.R` | `app_upload_file()` |
| `app-driver-window.R` | `app_set_window_size()`, `app_get_window_size()` |
| `app-driver-expect-*.R` | expectations and `get_*()` readers (see `snapshotExpectations.md`) |
| `app-driver-get-log.R`, `app-driver-log-message.R` | logs (see `loggingAndDebugging.md`) |
| `app-driver-variant.R` | `app_set_variant()`, `app_get_variant()` |
| `app-driver-message.R` | `app_abort()`, `app_warn()`, `app_inform()` |

- Every `app_*()` function starts with `ckm8_assert_app_driver(self, private)` (in `R/utils.R`). It checks that `self` is an `AppDriver` and `private` is an environment.
- `app_abort()` / `app_warn()` / `app_inform()` attach the driver to the condition (`app = self`), so a failed driver can be inspected with `rlang::last_error()$app`.
- `R/R6-helper.R` holds two small R6 helper classes used as private fields:
  - `Count`: an incrementing counter used by `app_next_temp_snapshot_path()` to name snapshot files `001`, `002`, ...
  - `Url`: stores the app URL. `$set()` validates it with `httr2::url_parse()` (scheme must be http/https, path defaults to `/`). `$combine()` joins a sub-path to the base URL and keeps the base query string.
- R files are loaded in the order listed in the `Collate:` field of `DESCRIPTION`. New files must be added there.

## Private state

Declared in the `private` list in `R/app-driver.R`:

- `chromote_session`: the `chromote::ChromoteSession`.
- `shiny_process`: the `callr::r_bg()` process. `NULL` when testing a remote URL.
- `shiny_proc_value`: the result of the background process, saved by `$stop()` when the app had already exited.
- `counter` (`Count`), `shiny_url` (`Url`).
- `load_timeout` (default 15000 ms), `timeout` (default 4000 ms). Real values are set by `app_init_timeouts()`.
- `logs`: list of one-row data frames added by `app_add_log_entry()`.
- `clean_logs`: delete stdout/stderr log files on finalize.
- `name`, `variant`: used in snapshot file names. `variant` holds `missing_value()` when not given (see `R/missing-value.R`) so R6 printing works.
- `state`: `"stopped"`, `"initialize"`, or `"running"`.
- `shiny_worker_id`: from `Shiny.shinyapp.config.workerId`, `NA` if empty. Used as a log column.
- `dir`: absolute app folder path, or a function.
- `save_dir`: temp folder for snapshot files before testthat copies them.
- `default_screenshot_args`, `default_expect_values_screenshot_args`.
- `shiny_test_url`: base URL of Shiny's test snapshot API, read from `Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl:true})`. Used by `get_values()` / `expect_values()`.
- `finalize`: calls `app_finalize()` on garbage collection.

## `$new()` flow

### `app_initialize()` (outer guard)

1. Calls `testthat::skip_on_cran()` unless env var `SHINYTEST2_APP_DRIVER_TEST_ON_CRAN` is `"true"` or `"1"`.
2. When `testthat::is_testing()`, tries to open a chromote session. If that fails, the test is skipped. On Windows CI it tries once silently first to "wake up" chromote.
3. Runs `app_initialize_()` inside `withCallingHandlers()`. On error it logs the message, calls `$view()` if interactive and `view` was not set, then aborts with the original message plus formatted `$get_logs()`.

### `app_initialize_()` (main work)

1. `gc()` so finalizers of old drivers stop their apps first.
2. Stores screenshot defaults, variant, `Count`, `Url`, timeouts, a fresh `save_dir`, `name`, `clean_logs`. Sets `state <- "initialize"`.
3. Resolves `app_dir`:
   - A `shiny.appobj` is saved to a temp folder by `app_save()` (`R/save-app.R`) and treated as a folder.
   - A string starting with `http://` or `https://` is a remote URL: `shiny_url$set()` and `app_set_dir(".")`. No process is started.
   - Otherwise `app_set_dir()` then `app_start_shiny()`.
4. Reads `inst/internal/js/shiny-tracer.js`.
5. Creates the browser tab: `chromote::default_chromote_object()$new_session()`.
6. If `view = TRUE`, calls `$view()`.
7. `app_init_browser_log()` subscribes to `Runtime.consoleAPICalled` and `Runtime.exceptionThrown` (and WebSocket events when `shiny.trace` is on) to fill logs.
8. If `height`/`width` given, sets the window size without waiting.
9. `Page$navigate()` to the app URL.
10. Evaluates `shiny-tracer.js` in the page with `chromote_eval()`.
11. Waits for `window.shinytest2.ready === true` with `load_timeout`. If `wait = TRUE`, also `wait_for_idle(duration = 200, timeout = load_timeout)`. Any error becomes "Shiny app did not become stable in ...ms".
12. Sets `state <- "running"`, reads `shiny_worker_id` and `shiny_test_url`.
13. If `check_names = TRUE`, runs `app_check_unique_names()` (warns on duplicate input/output IDs).

### `app_set_dir()` (`R/app-driver-dir.R`)

- Function input: stored as is. If the function's environment is the dev package namespace, it is wrapped in a new function that first calls `library(<pkg>)` (which the background process shims to `pkgload::load_all()`) and then runs the original function inside the package namespace.
- Path input: if a file path is given, its parent folder is used (`app_dir_value()`). The folder must exist.
- Errors when the folder has both `app.R` and a Shiny Rmd, or both `app.R` and `server.R`. Rmd plus `server.R` is allowed.
- `app_dir_rmd()` finds `.Rmd`/`.qmd` files not starting with `_` whose YAML `runtime` (or Quarto `server`) starts with `shiny`.

### `app_start_shiny()` (`R/app-driver-start.R`)

- Starts `callr::r_bg(supervise = TRUE)` with stdout/stderr going to temp `.log` files.
- `R_TESTS` is unset. For folder apps the working dir is set to the app folder, so the app's `.Rprofile` / `.Renviron` load.
- Inside the child process:
  - If `seed` is set, restores the parent's `RNGkind()`, calls `set.seed(seed)`, and sets Shiny's private seed to `seed + 11`.
  - Sets the user `options` plus `shiny.testmode = TRUE`.
  - If a dev package is detected (`dev_pkg_path()` / `dev_pkg_name()`, both turned off under `testthat::is_checking()`), it replaces `library()` and `require()` in the global env. The first call for that package runs `pkgload::load_all(export_all = FALSE)`. If the app folder is the package root, `shiny.autoload.r` is turned off.
  - Runs the app: a function is called (if it returns a `shiny.appobj`, that is run with `shiny::runApp()`); a Shiny doc runs with `rmarkdown::run(auto_reload = FALSE)`; otherwise `shiny::runApp()` with `shiny_args`.
  - The return value becomes the process result, which `$stop()` returns.
- Parent side: polls stderr 5 times a second (up to `load_timeout`) for `"Listening on http"`. Aborts with stderr content if the process dies or time runs out.
- Parses the URL from that line, then `app_wait_for_serving()` polls with `pingr::is_up()` until the port accepts connections. Shiny prints the line before it binds the port, so skipping this causes flaky "did not become stable" errors.

## `$stop()` flow (`R/app-driver-stop.R`)

`app_stop(signal_timeout)`:

1. Resolves `signal_timeout` (see `waitingAndTimeouts.md`; default 500 ms, 20 s under covr).
2. If `state == "stopped"`, returns the saved process value right away. Calling `$stop()` twice is safe.
3. Closes the chromote session (errors are logged, not thrown).
4. If there is a local process:
   - Alive: `interrupt()` (SIGINT), wait `signal_timeout`; `signal(SIGTERM)`, wait `signal_timeout`; `kill()`. The result is ignored. SIGINT first lets Shiny exit cleanly and lets tools like covr write results.
   - Already exited: stores `get_result()` in `shiny_proc_value`.
5. Sets `state <- "stopped"` and returns `shiny_proc_value` invisibly.

`app_finalize()` (on GC) calls `$stop()`, deletes the process log files when `clean_logs` is `TRUE`, and deletes `save_dir`.

## `$get_process()`

Returns `private$shiny_process` (a `callr::r_process`), or `NULL` in URL mode. Useful to check `is_alive()` or to wait for an app that calls `shiny::stopApp()` before calling `$stop()`.

## Interaction helpers

- `$click(input=)`: finds the node, then calls `set_inputs(<id> = "click")`, so it waits for outputs. The tracer turns `"click"` into a jQuery click for action buttons and `bslib` task buttons.
- `$click(output=)` / `$click(selector=)`: finds the node and runs `this.click()` on it through `Runtime.callFunctionOn`. It does not wait.
- `node_id_css_selector()` maps `input` to `#id.shiny-bound-input` and `output` to `#id.shiny-bound-output`. Exactly one of input/output/selector is allowed. No match aborts; more than one match warns and uses the first.
- `$set_inputs()`: queues values with `shinytest2.inputQueue.add()` and flushes them. See `waitingAndTimeouts.md`.
- `$upload_file()`: one named input only. Starts the output waiter for 2 messages, sets the file with `DOM.setFileInputFiles` using `fs::path_real()`, waits, then sleeps 0.6 s for the progress bar CSS transition.
- `$set_window_size()`: `Emulation.setDeviceMetricsOverride`, then `wait_for_idle()` unless `wait = FALSE`. `$get_window_size()` reads `Page.getLayoutMetrics()$cssLayoutViewport`.
- `$get_js()` waits for promises and returns the value. `$run_js()` does not wait or return a value. Both go through `chromote_eval()`, which wraps the script in `{ }` so `let`/`const` stay local.

## Local vs URL modes

| | Local (folder, file, app object, function) | URL |
|---|---|---|
| `private$shiny_process` | `callr` process | `NULL` |
| `$get_dir()` | absolute app path or function | absolute path of `"."` |
| `seed`, `options`, `shiny_args`, `render_args` | used | ignored |
| Shiny logs in `$get_logs()` | read from process files | none |
| `$stop()` | closes browser and kills process | closes browser only |

In URL mode the remote app must already run with test mode on, or `get_values()` will not work.

## Invariants / gotchas

- Keep the wrapper pattern: public methods in `app-driver.R` only forward to `app_*()` functions. Put logic in the topic files.
- `private$state` must be set to `"initialize"` early so a failed init can still be stopped. `$stop()` only short-circuits on `"stopped"`.
- `shiny-tracer.js` is injected once, after navigation, with `Runtime.evaluate`. A page reload removes it. There is a TODO to ship it as an HTML dependency instead.
- Do not remove `app_wait_for_serving()`; the "Listening on" line comes before the port is bound.
- `auto_reload = FALSE` for Rmd apps must stay; file polling breaks `wait_for_idle()` and floods logs.
- Under `R CMD check` (`testthat::is_checking()`), the dev package is not loaded from source; the installed copy is used.
- `chromote_eval()` asserts `awaitPromise = TRUE` and `returnByValue = TRUE` unless `allow_no_response = TRUE`. Only `$run_js()` should use `allow_no_response`.
