---
title: Snapshot Expectations
description: How AppDriver $expect_*() methods build snapshot files, name them, pick variants, compare screenshots with a threshold, and hand off to testthat. Read before changing any expect method, snapshot naming, screenshots, or compare_screenshot_threshold().
---

# Snapshot Expectations

Every `AppDriver$expect_*()` method ends in a testthat 3e snapshot call. shinytest2 builds the file or value, names it, and then calls `testthat::expect_snapshot_file()` or `testthat::expect_snapshot_value()` through two small wrappers in `R/expect-snapshot.R`.

## The two wrappers (`R/expect-snapshot.R`)

### `app__expect_snapshot_file()`

Used by `$expect_values()`, `$expect_screenshot()`, and `$expect_download()`.

- Sets `testthat::local_edition(3)`.
- Prefixes the file name with the AppDriver `name` (if set): `"<name>-<file>"`.
- Cleans the name with `fs::path_sanitize(name, "_")`.
- Calls `testthat::expect_snapshot_file(file, name =, cran = FALSE, compare =, transform =, variant = self$get_variant())`.
- On failure, if `compare` is `testthat::compare_file_text`, `{diffobj}` is installed, and option `shinytest2.expectation_failure.quiet` is not `TRUE`, it reads the old file and the `.new.` file from the active snapshotter and signals an extra `"warning"` expectation with a line diff (vdiffr style). The original failure is re-thrown first; the diff warning is deferred.

### `app__expect_snapshot_value()`

Used by `$expect_js()`, `$expect_text()`, `$expect_html()`, and the file name part of `$expect_download()`. Calls `testthat::expect_snapshot_value(x, cran = FALSE, variant = self$get_variant())`. These values land in the test file's `.md` snapshot, not in a separate file.

## Where snapshot files land

testthat decides the final path. With shinytest2's inputs it becomes:

```
tests/testthat/_snaps/[<variant>/]<test-file-stem>/[<app name>-]<file name>
```

- `<test-file-stem>`: test file name without `test-` and `.R` (e.g. `test-app-image.R` -> `app-image`).
- `<variant>` folder only exists when `get_variant()` is not `NULL`.
- Value snapshots (`expect_js` and friends) go into `_snaps/[<variant>/]<test-file-stem>.md`.
- On a mismatch testthat writes the new copy beside the old one as `<file>.new.<ext>`.

Real examples: `tests/testthat/_snaps/app-image/values-image-001.json`, `.../values-image-001_.png`, `tests/testthat/_snaps/app-download/001-download-link.txt`.

## File naming and the counter

### `app_next_temp_snapshot_path()` (`R/R6-helper.R`)

- Each AppDriver has one `Count` R6 object (`private$counter`, created in `app_initialize_()`).
- If `name` is `NULL`, the file name is `sprintf("%03d", private$counter$increment())`, e.g. `001`.
- If `name` is given, only `fs::path_file(name)` is used, and the counter is not bumped.
- The extension is forced with `fs::path_ext_set()` (`json` or `png`).
- The file is placed in `private$save_dir`, a temp dir made per AppDriver in `app_initialize_()` and deleted in `app_finalize()`.

The counter is shared by `$expect_values()`, `$expect_screenshot()`, and `$expect_download()`. Adding or removing one unnamed expectation renumbers all later unnamed snapshots in that AppDriver.

### AppDriver `name` and `known_app_driver_name_values()` (`R/record-test-unique-name.R`)

`AppDriver$new(name =)` becomes the prefix in `app__expect_snapshot_file()`. Two AppDrivers in the same test file with the same name (or both `NULL`) would write the same snapshot files. `known_app_driver_name_values()` parses a test file, finds `AppDriver$new()` / `shinytest2::AppDriver$new()` / `shinytest2:::AppDriver$new()` calls, and returns their literal `name` values (`NULL` included; symbols and expressions skipped). The recorder (`inst/internal/recorder/app.R`, `testname_validator`) uses it to reject a name already used in the target test file.

## `$expect_values()` (`R/app-driver-expect-values.R`)

`app_expect_values()`:

1. Picks a JSON path `NNN.json` (or `<name>.json`) and a debug PNG path `NNN_.png`.
2. Calls `testthat::announce_snapshot_file()` for both before anything can fail, so testthat does not delete them as unused if the test errors.
3. Builds the URL with `app_get_shiny_test_url(format = "json")`. With no `input`/`output`/`export` given, all three are requested. Given any one, the others are turned off. `sortC=1` is always added so keys are sorted the same way everywhere.
4. Fetches with `app_httr2_get()`. A 404 aborts with a hint about `test.mode = TRUE`.
5. `hash_snapshot_image_data()` replaces any base64 `data:image/...` string in the JSON with `[image data hash: <rlang::hash>]`. This keeps plot data out of the JSON.
6. `jsonlite::prettify(indent = 2)`.
7. Takes the debug screenshot (see below).
8. Writes the JSON and calls `app__expect_snapshot_file(compare = testthat::compare_file_text, transform =)`.

`$get_values()` / `$get_value()` use the same URL with `format = "rds"` and return R objects. `hash_images = TRUE` runs `hash_obj_images()` to hash images inside the list.

### The debug screenshot (`*_.png` and `*_.new.png`)

- `screenshot_args` defaults to `private$default_expect_values_screenshot_args` (from `AppDriver$new(expect_values_screenshot_args =)`, default `TRUE`).
- `default_screenshot_args()` turns missing, `NULL`, or `TRUE` into `list()`. `FALSE` skips the screenshot.
- If exactly one `output` is named, the selector defaults to `#<output>.shiny-bound-output` (user `screenshot_args` still win via `utils::modifyList()`).
- The screenshot goes through `app_expect_screenshot()` directly. This skips the variant check in `app_expect_screenshot_and_variant()`, but the file still uses `get_variant()` for its folder.
- All expectations from that call are muffled with `invokeRestart("muffle_expectation")`. A changed screenshot never fails `$expect_values()`. testthat still writes `NNN_.new.png` when it differs, so the user can see what changed.
- `use_shinytest2()` (`R/use.R`) adds `*_.new.png` to `.gitignore` and `_*.new.png` to `.Rbuildignore` for this reason.

## `$expect_screenshot()` (`R/app-driver-expect-screenshot.R`)

### Variant gate

`AppDriver$expect_screenshot()` calls `app_expect_screenshot_and_variant()`. If no `variant` was given at init (`app_is_missing_variant()`), it aborts and suggests `variant = platform_variant()`. `variant = NULL` is allowed but noted as fragile.

### Flow

`app_expect_screenshot()`:

1. Builds a `compare` function: by default `compare_screenshot_threshold(old, new, threshold =, kernel_size =, quiet =)`. A user `compare` replaces it.
2. Gets `NNN.png` from `app_next_temp_snapshot_path()` and announces it.
3. Calls `$get_screenshot(file = ...)`.
4. Calls `app__expect_snapshot_file(compare = compare)`.

Defaults for `threshold` and `kernel_size` come from options `shinytest2.compare_screenshot.threshold` (`NULL`) and `shinytest2.compare_screenshot.kernel_size` (`5`).

### `app_get_screenshot()` and the selector

- `screenshot_args` defaults to `private$default_screenshot_args` (from `AppDriver$new(screenshot_args =)`). `FALSE` is not allowed here; it warns and uses `list()`.
- `delay` defaults to `0`.
- `selector` defaults to `"scrollable_area"`. Before v0.3.0 the default was `"html"`, which gave odd sizes when the page had non-standard CSS height or width (NEWS, [rstudio/shinytest2#325](https://github.com/rstudio/shinytest2/issues/325)).
- `handle_custom_selector()` turns the two custom selectors into a chromote `cliprect`:
  - `"scrollable_area"`: `c(0, 0, html.scrollWidth, html.scrollHeight)` read with `$get_js()`.
  - `"viewport"`: current `window.scrollX/scrollY` plus `$get_window_size()`.
  - Any other string is passed to chromote as a CSS selector.
- Calls `self$get_chromote_session()$screenshot` via `do.call()` with the args and a temp file name.
- On Windows, `normalize_png_res_header()` (`R/utils.R`) rewrites the PNG `pHYs` header to 72 ppi so screen DPI does not change the file bytes.
- With `file = NULL` it shows the image via `{showimage}` instead of saving.

## Screenshot comparison (`R/compare-screenshot-threshold.R`, `src/code.cpp`)

### `compare_screenshot_threshold()`

1. `testthat::compare_file_binary()`. Same bytes -> `TRUE`.
2. `threshold` is `NULL` -> return that binary result (so the default is plain binary compare).
3. Check `threshold` is in `[0, 4 * kernel_size^2]`.
4. Call `screenshot_max_difference()` inside `try()`. Any error (different sizes, kernel too big) -> `FALSE`.
5. Return `max_value < threshold`. When `FALSE` and `quiet = FALSE`, it prints the max value, the threshold, and both paths.

### `screenshot_max_difference()`

- If `new` is missing, uses `<old>.new.png`. So after a failed test, `screenshot_max_difference("tests/testthat/_snaps/.../001.png")` shows how far apart the two files are.
- Reads both PNGs with `png::readPNG()` (values 0 to 1 per channel).
- Aborts if dimensions differ or `kernel_size` is not smaller than the image.
- `diff_matrix <- rowSums(abs(old - new), dims = 2)`: per pixel, the sum of channel differences (max 4 with RGBA).
- Calls C++ `image_diff_convolution_max_value(diff_matrix, kernel_size)`.

### `image_diff_convolution_max_value()` (C++, `src/code.cpp`)

Finds the largest sum of any `kernel_size x kernel_size` block of the diff matrix. The kernel is all 1s, so it is done as two rolling sums instead of a full 2D convolution:

1. Row pass: rolling sum of width `k` along each row, stored in `conv_matrix` (`nrow x (ncol - k + 1)`).
2. Column pass: rolling sum of height `k` down each column of `conv_matrix`, keeping only the max.

Cost is about `2 * rows * cols` instead of `rows * cols * k^2`. Partial blocks at the edges are skipped; since all values are non-negative, a partial block can never be larger than a full one. Exported to R through `R/cpp11.R` / `src/cpp11.cpp` (generated by cpp11).

## Variants (`R/app-driver-variant.R`, `R/platform.R`)

- `app_set_variant()`: missing stays a `missing_value()` marker; `FALSE` becomes `NULL`; other values are kept.
- `app_get_variant()`: returns `NULL` when missing. This is what both wrappers pass to testthat.
- `platform_variant(os_name = TRUE, r_version = TRUE)` returns e.g. `"mac-4.1"`. `os_name()` maps `Sys.info()` sysname to `mac`, `linux`, `solaris`, or `.Platform$OS.type`. `r_version()` is `major.minor` with no patch. Both are cached with `cache_fn_val()`. Both flags `FALSE` aborts.

## Other expectations

### `$expect_js()`, `$expect_text()`, `$expect_html()` (`R/app-driver-expect-js.R`)

- `app_expect_js()` runs `$get_js()` (awaits promises, returns by value), applies `pre_snapshot` if it is a function, then `app__expect_snapshot_value()`. A value snapshot is used because printed output differs across R versions and locales.
- `app_expect_text()` uses `get_text_js()`: `textContent` of every `document.querySelectorAll(selector)` match, with `pre_snapshot = unlist`. Input values are not in `textContent`.
- `app_expect_html()` uses `get_html_js()`: `outerHTML` (default) or `innerHTML`, with `pre_snapshot = unlist`.
- These do not use the counter or the `name` prefix; testthat stores them in order inside the `.md` file.

### `$expect_download()` (`R/app-driver-expect-download.R`)

`app_download(use_counter = TRUE, save_dir = private$save_dir)`:

- Waits (up to `private$timeout`) for `#<output>` to exist with a non-empty `href`, then reads the `href` and fetches it with `private$shiny_url$combine()` so base URL query params are kept.
- File name, in order: user `name`; the `filename=` from the `Content-Disposition` header; else `.download`.
- Always prefixed with the counter: `NNN-<name>` or `NNN.download`.
- Cleaned with `fs::path_sanitize()`.

Then `app__expect_snapshot_file(compare = NULL, transform =)` (testthat picks text or binary compare from the extension), and, if the header gave a file name, `app__expect_snapshot_value()` on that name. The snapshot file cannot be announced up front because the name comes from the response.

`$get_download()` uses `use_counter = FALSE` and saves to `tempdir()` or the given path; no snapshot.

### `$expect_unique_names()` (`R/app-driver-expect-unique-names.R`)

`testthat::expect_warning(app_check_unique_names(), NA)`. `app_check_unique_names()` calls `shinytest2.listComponents()` in the browser and warns on duplicate ids among inputs, outputs, and ids used by both. The same check runs at init when `check_names = TRUE`, as a warning only.

## Invariants / gotchas

- Always announce snapshot files before work that can fail (`announce_snapshot_file()`), or testthat will delete existing snapshots as unused when the test errors.
- Unnamed `expect_values` / `expect_screenshot` / `expect_download` share one counter per AppDriver. Inserting an expectation renames all later files.
- `$expect_screenshot()` needs a `variant` at init; the debug screenshot inside `$expect_values()` does not.
- The default screenshot compare is binary. A `threshold` must be set (argument or option) for fuzzy matching.
- `threshold` larger than `4 * kernel_size^2` errors in `checkmate::assert_double()`.
- Debug screenshot failures are muffled on purpose. Do not let them fail `$expect_values()`. testthat 3.3.0 changed how screenshot expectations behave here (NEWS, [rstudio/shinytest2#418](https://github.com/rstudio/shinytest2/issues/418)).
- All snapshot calls pass `cran = FALSE`. AppDriver tests are skipped on CRAN anyway (`app_initialize()`).
- The `cran =` argument on expect methods is deprecated (`check_cran_deprecated()` in `R/app-driver.R`).
- Images in `$expect_values()` JSON are hashes. A plot change shows as a hash change, not a readable diff; look at the `_.new.png`.
- Snapshot files in `private$save_dir` are not removed on `$stop()`; they are removed in `app_finalize()` because testthat may still need them during the test.
