---
title: Waiting, shiny-tracer.js, and timeouts
description: Read when changing or debugging wait_for_idle, wait_for_value, wait_for_js, how set_inputs/upload_file wait for outputs, the injected shiny-tracer.js, or how timeout defaults, options, and env vars are resolved.
---

# Waiting, shiny-tracer.js, and timeouts

Shiny updates happen over a WebSocket, so a test must wait for the app to settle before it reads values. shinytest2 does this with a small JS script injected into the page plus a few R wait methods.

## shiny-tracer.js

File: `inst/internal/js/shiny-tracer.js`. It is evaluated in the page right after navigation by `app_initialize_()` (see `appDriver.md`). It creates one global, `window.shinytest2`.

### Fields

- `ready`: set to `true` once jQuery is loaded and the Shiny session is connected (either `Shiny.shinyapp.config.sessionId` already exists, or on the `shiny:sessioninitialized` event). It does not wait for first output; startup relies on `wait_for_idle()` for that.
- `busy`: `null` at start, then `true` on `shiny:busy` and `false` on `shiny:idle`.
- `updating`: list of output names; a name is removed when its `shiny:value` event fires.
- `connected`, `log_entries`, `entries_shown`, `log_messages`: declared but not used by current code paths.

### Functions

- `shinytest2.log(msg)`: `console.info("shinytest2;", msg)`. chromote captures it into `$get_logs()`.
- `shinytest2.inputQueue`:
  - `add(inputs)`: pushes `{name, value, allowNoInputBinding, priority}` items.
  - `flush()`: for each item, finds the input binding (`$el.data("shinyInputBinding")`), runs a preprocessor, calls `binding.setValue()`, and triggers `change`. With no binding, it calls `Shiny.setInputValue()` only if `allowNoInputBinding` is true, else it throws. The queue is always cleared.
  - `preprocessors` by binding name: `shiny.dateRangeInput` (array to `{start, end}`), `shiny.actionButtonInput` and `bslib.task-button` (only `"click"`, triggers a click), `shiny.fileInputBinding` (always throws; use `upload_file()`), `shiny.sliderInput` (date strings to epoch ms).
- `shinytest2.outputValuesWaiter`: `start(timeout, n_messages = 1)` and `finish(wait, callback)`. It counts `shiny:message` events whose message has a `values` field (server sent output values). When enough arrive, or `timeout` passes, it calls `callback({timedOut})` on the next tick so outputs are already applied. `start()` throws if a wait is already pending.
- `shinytest2.listComponents()`: lists IDs of bound inputs and outputs, used for duplicate ID checks.
- `shinytest2.getAllValues()`: returns `Shiny.shinyapp.$inputValues`, `$values`, `$errors`.

## Wait methods (`R/app-driver-wait.R`)

All three resolve `timeout` with `app_get_timeout()`: the given value, or the driver's resolved `private$timeout`.

### `$wait_for_js(script, timeout, interval = 100)`

- `app_wait_for_js()` calls `chromote_wait_for_condition()` in `R/chromote-methods.R`.
- That builds a JS Promise that runs `eval(<script>)` every `interval` ms until it is truthy. The script text is escaped with `deparse()` so it runs as is.
- The timeout is tracked in JS (`Date.now()`), because chromote cannot cancel an already-scheduled `setTimeout`. The CDP call gets `timeout * 2` so the JS timeout fires first.
- A JS timeout becomes the R error "Timed out waiting for JavaScript script to return `true`". Any other JS error becomes "Error found while waiting for JavaScript script to return `true`." with the exception.

### `$wait_for_idle(duration = 500, timeout)`

- `app_wait_for_idle()` runs a JS Promise in the page:
  - On `shiny:idle`, start a `duration` ms timer. On `shiny:busy`, cancel it.
  - When the timer finishes, resolve. If `timeout` passes first, reject.
  - If `shinytest2.busy !== true` at the start, it treats the app as already idle and starts the timer.
- Listeners are removed on resolve or reject.
- CDP gets `timeout * 2`. Any error aborts with "An error occurred while waiting for Shiny to be stable".
- Used by: `$new()` (with `duration = 200` and `load_timeout`, when `wait = TRUE`), and `$set_window_size()` (default `wait = TRUE`).

### `$wait_for_value(input/output/export, ignore = list(NULL, ""), timeout, interval = 400)`

- Runs in R, not JS. Exactly one of `input`, `output`, `export` must be given (`app_get_single_ioe()`).
- Loops: call `$get_value()`; if it does not error and is not `identical()` to any item in `ignore`, return the value. Otherwise sleep `interval` ms. Abort with "timeout reached when waiting for ..." when time runs out.
- Errors from `$get_value()` are swallowed and retried until timeout.

## How `$set_inputs()` waits (`R/app-driver-set-inputs.R`)

1. Each named value becomes `{value, allowNoInputBinding, priority}`.
2. `app_queue_inputs()` runs `shinytest2.inputQueue.add(...)` with `$run_js()`.
3. `app_flush_inputs()` runs this with `$get_js()` (CDP timeout `2 * timeout_`):
   `outputValuesWaiter.start(timeout_)` then `inputQueue.flush()` then `outputValuesWaiter.finish(wait_, resolve)`.
4. Results:
   - A string result means a JS error (for example, no input binding). It is logged and shown as a message; it does not throw.
   - `timedOut = TRUE` shows a message: "Server did not update any output values within N seconds. If this is expected, use `wait_ = FALSE` or increase the value of `timeout_`." It does not throw.
- `wait_ = FALSE` returns as soon as inputs are flushed.
- It waits for one output-value message, not for full idle. If an input causes several rounds of updates, follow with `$wait_for_idle()` or `$wait_for_value()`.
- `$click(input=)` goes through `set_inputs()`, so it gets the same wait.

`$upload_file()` uses the same waiter but calls `start(timeout_, 2)` because an upload sends two messages. It then sleeps 0.6 s for the progress bar transition.

## Timeout resolution (`R/app-driver-timeout.R`)

`timeout_value()` picks the first non-`NULL` value:

1. The argument passed by the user.
2. R option.
3. Environment variable (must parse as a number).
4. Built-in default.

Each value must be a single finite number `>= 0` (`assert_timeout()`). All values are milliseconds.

| Setting | Argument | Option | Env var | Default |
|---|---|---|---|---|
| Load | `AppDriver$new(load_timeout=)` | `shinytest2.load_timeout` | `SHINYTEST2_LOAD_TIMEOUT` | 15000 |
| General | `AppDriver$new(timeout=)` | `shinytest2.timeout` | `SHINYTEST2_TIMEOUT` | 4000 |
| Stop signals | `$stop(signal_timeout=)` | `shinytest2.signal_timeout` | `SHINYTEST2_SIGNAL_TIMEOUT` | 500, or 20000 when `R_COVR=true` |

- `app_init_timeouts()` resolves load and general timeouts once in `$new()` and stores them in `private$load_timeout` and `private$timeout`. Changing an option later does not affect an existing driver.
- `app_get_timeout()` is the per-call lookup: the method's `timeout` argument if given, else `private$timeout`. Methods using it include `wait_for_*()`, `set_inputs()`, `upload_file()`, `get_js()`, `run_js()`, `expect_js()`.
- `load_timeout` covers: polling for `"Listening on http"` in `app_start_shiny()`, `app_wait_for_serving()`, waiting for `shinytest2.ready`, and the startup `wait_for_idle()`.
- `signal_timeout` is resolved per `$stop()` call and does not use the driver's `timeout`.
- `chromote_eval()` has its own default CDP timeout of 4000 ms, used only when a caller does not pass one.

### CI timeouts for this package's tests

`tests/testthat/setup-ci-timeouts.R` runs before the package tests. When `on_ci()` is true (env var `CI` is truthy, `R/utils.R`), it sets `SHINYTEST2_LOAD_TIMEOUT=60000` and `SHINYTEST2_TIMEOUT=20000` unless already set. It uses env vars, not options, so child R processes from `callr::rscript()` inherit them. This applies only to shinytest2's own test suite, not to users. There is no multiplier logic; the values are fixed.

`tests/testthat/test-app-timeout.R` and `tests/testthat/test-stop.R` check the resolution order.

## Invariants / gotchas

- Only one `outputValuesWaiter` wait can be pending at a time; `start()` throws otherwise.
- JS-side waits pass `2 * timeout` to CDP so the JS timer, which gives a clear error, fires before chromote's timeout.
- `wait_for_idle()` treats `busy === null` (no busy/idle event seen yet) as idle.
- `set_inputs()` timeouts and binding errors are messages, not errors. Tests that need a hard failure must check values after.
- The tracer lives only until the page reloads. After a reload, `window.shinytest2` is gone and all these waits fail.
- Keep the env var names and option names stable; users set them in CI.
