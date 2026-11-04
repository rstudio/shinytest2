---
title: Logging and Debugging
description: How AppDriver collects logs from shinytest2, the Shiny process, and the browser (console, exceptions, WebSocket traffic), how logs print, and how to debug a failing AppDriver with $get_logs(), $view(), and $get_chromote_session(). Read when changing logging code or chasing a test failure.
---

# Logging and Debugging

An AppDriver keeps one log that merges three sources. `$get_logs()` returns it as a `data.frame` with class `shinytest2_log`, which prints as aligned, colored lines.

Files:

- `R/app-driver-log-message.R`: building and storing log entries; `app_get_logs()`.
- `R/app-driver-get-log.R`: browser event listeners and reading the Shiny process log files.
- `R/shinytest2-logs.R`: `format()` / `print()` methods for `shinytest2_log`.
- `R/app-driver-chromote.R`: `$get_chromote_session()` and `$view()`.
- `R/shiny-browser.R`: `with_external_shiny_browser()` (used by the recorder).

## Log entry shape

`app_log_entry()` makes a one-row `data.frame`:

| column | value |
|---|---|
| `workerid` | `private$shiny_worker_id` (`NA` until read from `Shiny.shinyapp.config.workerId` at the end of init; `""` is turned into `NA`) |
| `timestamp` | `Sys.time()` by default; `NA` for Shiny process lines |
| `location` | one of `"chromote"`, `"shiny"`, `"shinytest2"` (`match.arg()`) |
| `level` | any single string; not limited to a fixed set so any `console.<level>()` works |
| `message` | single string |

`app_add_log_entry()` appends the row to `private$logs` (a list). Rows are only bound together when `$get_logs()` is called.

## The three sources

### 1. `"shinytest2"`: R side messages

`$log_message(message)` -> `app_log_message()` -> `app_add_log_entry(location = "shinytest2", level = "info")`. The package calls `self$log_message()` at each step of init ("Start AppDriver initialization", "Creating new ChromoteSession", "Injecting shiny-tracer.js", ...), and in methods like `$get_values()`, `$expect_values()`, `$get_screenshot()`, `$stop()`, and downloads. Users can add their own markers with `app$log_message()`.

### 2. `"shiny"`: background R process output

`app_start_shiny()` (`R/app-driver-start.R`) starts the app with `callr::r_bg()` and sends stdout and stderr to temp files named `shiny-stdout-*.log` and `shiny-stderr-*.log`.

`app_make_shiny_log()` reads both files every time `$get_logs()` runs:

- stderr lines first, then stdout lines.
- `timestamp = NA` (the files have no times).
- `filter_log_text()` removes `\f` (it clears the RStudio console).
- Returns `NULL` when there is no local process (app given as a URL).

`message()` output and Shiny's own startup lines ("Listening on http://...") go to stderr.

### 3. `"chromote"`: browser events

`app_init_browser_log()` registers chromote event callbacks. It runs in `app_initialize_()` right after the `ChromoteSession` is created and before `Page$navigate()`, so page load messages are caught.

- `Runtime$consoleAPICalled` -> `level = info$type` (`log`, `info`, `warn`, `error`, `debug`, ...). `console_api_to_msg()` joins the args turned into text by `obj_to_string()` and adds a stack trace from `frames_to_msg()`.
- `Runtime$exceptionThrown` -> `level = "throw"`. `exception_thrown_to_msg()` gives the exception text, the error description, and the stack.
- WebSocket frames, only when `isTRUE(options$shiny.trace)` (see below): `Network$webSocketFrameSent` -> `"send <payload>"`, `Network$webSocketFrameReceived` -> `"recv <payload>"`, both `level = "websocket"`. Base64 data is shortened to `[base64 data]`.

#### `shiny-tracer.js` messages

`inst/internal/js/shiny-tracer.js` defines `shinytest2.log(message)` as `console.info("shinytest2;", message)`. So tracer messages show up as `{chromote} JS info ... shinytest2; <msg>` (e.g. `Connected`, `shiny:busy`, `shiny:idle`, `shiny:value distPlot`). The wait helpers in `R/app-driver-wait.R` also call `window.shinytest2.log()`. `frames_to_msg()` drops the stack trace for these `info` calls when the first frame is `window.shinytest2.shinytest2.log`, to keep them on one line.

#### `obj_to_string()`

Turns a CDP `RemoteObject` into text: booleans, strings, numbers, `undefined`, `bigint`, functions/symbols (description), errors (description), arrays (preview values, `...` if cut off), `null`, other objects as `[object ClassName]`. An unknown type prints a one-time message asking for a bug report and returns `"(unknown)"`.

## `shiny.trace`

Pass it through `AppDriver$new(options = list(shiny.trace = TRUE))`. That one setting does two things:

1. `app_init_browser_log()` reads `options$shiny.trace` and turns on the WebSocket listeners (`{chromote}` `websocket` rows).
2. `app_start_shiny()` sets the same `options` inside the app process (`do.call(base::options, .options)`, along with `shiny.testmode = TRUE`), so Shiny prints its own `SEND` / `RECV` lines to stderr (`{shiny}` `stderr` rows).

Setting `options(shiny.trace = TRUE)` in the test session alone does not turn on the browser WebSocket logging; it has to be in the `options` argument.

## `$get_logs()` and printing

`app_get_logs()` row-binds `private$logs` and the Shiny file lines, orders by `timestamp`, and adds class `shinytest2_log`. `order()` puts `NA` last, so all `{shiny}` lines appear at the end, not mixed in time order.

`format.shinytest2_log()` (`R/shinytest2-logs.R`) prints each row as:

```
{location} LANG level HH:MM:SS.ss message
```

- `LANG` is `JS` for chromote and `R` for shiny/shinytest2.
- `NA` timestamps print as `-----------`.
- Colors: shiny stderr magenta; chromote `throw`/`error` red, other chromote cyan; shinytest2 green.
- Multi-line messages (stack traces) are indented to line up under the first line, and each line is colored on its own.

The result is still a `data.frame`, so `subset(logs, level == "websocket")` or `subset(logs, location == "chromote")` works.

## Viewing the browser

### `$view()` and `$get_chromote_session()` (`R/app-driver-chromote.R`)

- `$get_chromote_session()` returns `private$chromote_session`, the raw `chromote::ChromoteSession`. Use it for any CDP call shinytest2 does not wrap, e.g. `b$Runtime$evaluate("1 + 1")`.
- `$view()` logs "Viewing chromote session" and calls `ChromoteSession$view()`, which opens the headless page in a local browser tab so you can watch it.
- `AppDriver$new(view = TRUE)` calls `$view()` before navigating to the app.

### Automatic view on init failure (`app_initialize()`, `R/app-driver-initialize.R`)

If `app_initialize_()` errors:

1. It logs `"Error while initializing AppDriver:\n<message>"`.
2. In an interactive session, if a `ChromoteSession` exists and `view` was not given (`TRUE` already opened it; `FALSE` means do not), it calls `$view()` for you.
3. It aborts with the original message plus the full formatted `$get_logs()` output, and a hint: `rlang::last_error()$app` holds the failed AppDriver.

All `app_abort()` / `app_warn()` / `app_inform()` calls (`R/app-driver-message.R`) attach `app = self` to the condition, which is how `rlang::last_error()$app` works.

### `with_external_shiny_browser()` (`R/shiny-browser.R`)

Not part of AppDriver logging. `record_test()` (`R/record-test.R`) wraps `shiny::runApp()` of the recorder in it so the recorder opens in a real browser:

- Not RStudio Server and chromote's default browser is a local Chrome: temporarily sets `shiny.launch.browser` to open that Chrome path (shell quoted except on Windows).
- Else on RStudio Desktop: temporarily sets the RStudio preference `shiny_viewer_type` to `"browser"` (or uses `.rs.invokeShinyWindowExternal` on old RStudio).
- RStudio Server: no change.

## Log file cleanup

- `AppDriver$new(clean_logs = TRUE)` (default) stores `private$clean_logs`.
- `app_finalize()` (`R/app-driver-stop.R`, run on garbage collection) calls `$stop()` and, if `clean_logs`, deletes the stdout/stderr files.
- `$stop()` does not delete them. `$get_logs()` after `$stop()` still includes the Shiny lines. After finalize they are gone.

## Debugging tips

- Call `app$get_logs()` first. Look for `{chromote} JS throw` / `error` rows (browser errors) and `{shiny} R stderr` rows (R errors in the app).
- The `shinytest2;` info rows show where init or a wait stopped: e.g. `Waiting for shiny session to connect` without `Connected` means the WebSocket never connected.
- Add `app$log_message("before click")` between steps to find which step a browser error belongs to.
- Use `options = list(shiny.trace = TRUE)` to see every message between browser and server, from both sides.
- Use `AppDriver$new(view = TRUE)` or `app$view()` to watch the page live. In interactive sessions a failed init opens the view for you.
- After an init error, inspect `rlang::last_error()$app` (e.g. `$get_logs()`, `$get_screenshot()`).
- Use `app$get_chromote_session()` for raw CDP calls, and `app$get_process()` for the `callr` process (check `$is_alive()`, read its output files).
- Keep `clean_logs = FALSE` if you need the raw `shiny-stdout-*.log` / `shiny-stderr-*.log` files after the AppDriver is collected.

## Invariants / gotchas

- Browser listeners must be registered before `Page$navigate()`; moving `app_init_browser_log()` later loses page load messages.
- `level` is not checked against a fixed list on purpose (e.g. `console.trace()` levels).
- The `get_logs` roxygen docs in `R/app-driver.R` say the `shinytest2` level is `"log"`, but `app_log_message()` writes `"info"`.
- Shiny process rows have no timestamp and always sort to the end, so you cannot read exact timing between `{shiny}` lines and other rows.
- WebSocket logging is decided once at init from the `options` argument; it cannot be turned on later.
- Log rows are kept in memory (`private$logs`) for the life of the AppDriver; with `shiny.trace` on, this list can get large.
