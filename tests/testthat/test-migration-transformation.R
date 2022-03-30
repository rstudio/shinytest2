expect_msg_helper <- function(expr, message, fixed, expect_fn, ...) {
  if (is.na(message)) {
    if (interactive()) {
      force(expr)
    } else {
      testthat::expect_silent(expr)
    }
  } else {
    expect_fn(expr, regexp = message, fixed = fixed, ...)
  }
}

expect_migration <- function(
  original_expr,
  new_expr,
  message = NA,
  ...,
  fixed = TRUE,
  enexpr_new_expr = TRUE,
  info_env = make_info_env(verbose = !is.na(message))
) {
  expect_msg_helper(
    migrated_expr <-
      m__shinytest_lang(
        rlang::enexpr(original_expr),
        info_env
      ),
    message = message,
    fixed = fixed,
    expect_fn = testthat::expect_message,
    ...
  )

  shinytest2_expr <-
    if (enexpr_new_expr) {
      rlang::enexpr(new_expr)
    } else {
      new_expr
    }
  testthat::expect_equal(migrated_expr, shinytest2_expr)

  # Return the environment as that may have been altered
  invisible(info_env)
}

expect_migration_error <- function(
  original_expr,
  message = NA,
  ...,
  fixed = TRUE,
  info_env = make_info_env()
) {
  expect_msg_helper(
    migrated_expr <-
      m__shinytest_lang(
        rlang::enexpr(original_expr),
        info_env
      ),
    message = message,
    fixed = fixed,
    expect_fn = testthat::expect_error
  )

  invisible(info_env)
}



test_that("setInputs is converted", {
  expect_migration(
    app$setInputs(x = 1, allowInputNoBinding_ = TRUE, y = 2),
    app$set_inputs(x = 1, y = 2, allow_no_input_binding_ = TRUE)
  )
  expect_migration_error(
    app <- app$setInputs(x = 1, allowInputNoBinding_ = TRUE, y = 2),
    "Use `AppDriver$get_values()` directly."
  )
  expect_migration_error(
    local({app$setInputs(x = 1, allowInputNoBinding_ = TRUE, y = 2)}),
    "Use `AppDriver$get_values()` directly."
  )

  expect_migration_error(
    app$setInputs(x = 1),
    NA
  )
  expect_migration_error(
    app$setInputs(x = 1, values_ = FALSE),
    NA
  )
  expect_migration_error(
    app$setInputs(x = 1, values_ = TRUE),
    "Use `AppDriver$get_values()` directly."
  )
})


test_that("click is converted", {
  expect_migration(
    app$click("mybutton"),
    app$click(selector = "#mybutton")
  )
  expect_migration(
    app$click(mybutton),
    app$click(selector = paste0("#", mybutton))
  )
  expect_migration(
    app$click("mybutton", iotype = "input"),
    app$click(input = "mybutton")
  )
  expect_migration(
    app$click("myoutput", iotype = "output"),
    app$click(output = "myoutput")
  )
  expect_migration(
    app$click("my_id", iotype = "auto"),
    app$click(selector = "#my_id")
  )
})



test_that("executeScript is converted", {
  expect_migration(
    app$executeScript("1 + 1"),
    app$get_js(script = "1 + 1", timeout = 10000)
  )
  expect_migration(
    app$executeScript("1 + 1", x = 1, y = 2),
    app$get_js(script = "let arguments_ = {\"x\":1,\"y\":2};\n1 + 1", timeout = 10000)
  )
})

test_that("executeScriptAsync is converted", {
  expect_migration_error(
    app$executeScriptAsync("var callback = aruguments[0]; callback()"),
    "vignette for an example"
  )
})

test_that("expectUpdate is converted", {
  expect_migration(
    app$expectUpdate("myoutput", myinput = 42),
    local({
      prior_output_value <- app$get_value(output = "myoutput")
      app$set_inputs(myinput = 42, timeout_ = 3000)
      new_output_value <- app$get_value(output = "myoutput")
      testthat::expect_failure(
        testthat::expect_equal(
          new_output_value,
          prior_output_value
        )
      )
    }),
    "Using `iotype = \"input\"`"
  )
  expect_migration_error(
    app$expectUpdate("otherinput", myinput = 42, iotype = "output"),
    "`ShinyDriver$expectUpdate(iotype = \"output\")` is not supported"
  )
})

test_that("deprecated methods are covered", {
  # Use ... to avoid issues with enexpr / evaluations
  expect_deprecated_error <- function(...) {
    expect_migration_error(
      ...,
      "Please see the `shinytest-migration` vignette"
    )
  }
  expect_deprecated_error(app$findElement(".myclass"))
  expect_deprecated_error(app$findElements(".myclass"))
  expect_deprecated_error(app$findWidget("myinput"))
})


test_that("getAllValues is converted", {
  expect_migration(
    app$getAllValues(),
    app$get_values()
  )
  expect_migration(
    app$getAllValues("myinput"),
    app$get_values(input = "myinput")
  )
  expect_migration(
    app$getAllValues(output = "myoutput", "myinput", "myexport"),
    app$get_values(input = "myinput", output = "myoutput", export = "myexport")
  )
})


test_that("isRmd, getAppDir, getAppFilename is converted", {
  expect_migration(
    app$isRmd(),
    length(fs::dir_ls(app$get_dir(), regexp = "\\.[Rr]md$")) > 0
  )
  expect_migration(
    app$getAppDir(),
    app$get_dir()
  )
  expect_migration(
    app$getAppFilename(),
    local({
      rmd_paths <- fs::dir_ls(app$get_dir(), regexp = "\\.[Rr]md$")
      is_rmd <- length(rmd_paths) > 0
      if (is_rmd) {
        fs::path_file(rmd_paths[1])
      } else {
        NULL
      }
    })
  )
})


test_that("enableDebugLogMessages, getDebugLog, getEventLog is converted", {
  expect_migration(
    app$enableDebugLogMessages(),
    NULL,
    "`ShinyDriver$enableDebugLogMessages()` is not implemented"
  )
  expect_migration(
    app$getDebugLog(),
    app$get_logs(),
    "A single `AppDriver$get_logs()`"
  )
  expect_migration(
    app$getEventLog(),
    app$get_logs(),
    "A single `AppDriver$get_logs()`"
  )
})


test_that("getRelativePathToApp, getTestsDir, getSnapshotDir is converted", {
  expect_migration_error(
    app$getRelativePathToApp(),
    "{shinytest2} is integrated with {testthat}"
  )
  expect_migration_error(
    app$getTestsDir(),
    "{shinytest2} is integrated with {testthat}"
  )
  expect_migration_error(
    app$getSnapshotDir(),
    "{shinytest2} is integrated with {testthat}"
  )
})


test_that("getSource, getTitle is converted", {
  expect_migration(
    app$getSource(),
    app$get_html("html", outer_html = TRUE)
  )
  expect_migration(
    app$getTitle(),
    app$get_js("window.document.title;")
  )
})


test_that("getUrl is converted", {
  expect_migration(
    app$getUrl(),
    app$get_url()
  )
})

test_that("getValue is converted", {
  # While one message is found, a second message may not be captured. Only validate one of them. (Nesting is odd.)
  suppressMessages({
    expect_migration_error(
      app$getValue("myoutput", iotype = iotype),
      "must be a character value, not a variable"
    )
    expect_migration(
      app$getValue("myoutput", iotype = "output"),
      app$get_value(output = "myoutput")
    )
    expect_migration(
      app$getValue("myoutput", iotype = "auto"),
      app$get_value(output = "myoutput"),
      "Using `output` as a guess."
    )
    expect_migration(
      app$getValue("myoutput"),
      app$get_value(output = "myoutput"),
      "Using `output` as a guess."
    )
  })

  expect_migration(
    app$getValue("myinput", iotype = "input"),
    app$get_value(input = "myinput")
  )
})

test_that("setValue is converted", {
  # While one message is found, a second message may not be captured. Only validate one of them. (Nesting is odd.)
  suppressMessages({
    expect_migration(
      app$setValue("myinput", 42),
      app$set_inputs("myinput" = 42),
      "`ShinyDriver$setValue()` is not implemented in `AppDriver`"
    )
    expect_migration(
      app$setValue("myinput", 42),
      app$set_inputs("myinput" = 42),
      "Using `input` as a guess."
    )
    expect_migration(
      app$setValue("myinput", 42, iotype = "input"),
      app$set_inputs("myinput" = 42),
      NA
    )
    expect_migration_error(
      app$setValue("myinput", 42, iotype = "output"),
      "`ShinyDriver$setValue(iotype=)` is set to `\"output\"`"
    )
    expect_migration_error(
      app$setValue("myinput", 42, iotype = output),
      "must be a character value, not a variable"
    )
  })
})


test_that("getWindowSize, setWindowSize is converted", {
  expect_migration(
    app$getWindowSize(),
    app$get_window_size()
  )
  expect_migration(
    app$setWindowSize(height = 100, 200),
    app$set_window_size(width = 200, height = 100)
  )
  expect_migration(
    app$setWindowSize(100, 200),
    app$set_window_size(width = 100, height = 200)
  )
})


test_that("goBack, refresh is converted", {
  expect_migration(
    app$goBack(),
    app$run_js("window.history.back();")
  )
  expect_migration(
    app$refresh(),
    app$run_js("window.location.reload();")
  )
})


test_that("clone is converted", {
  expect_migration_error(
    app$clone(),
    "`AppDriver$clone()` is not supported."
  )
})


test_that("listWidgets is converted", {
  expect_migration(
    app$listWidgets(),
    lapply(app$get_values(), names)
  )
})


test_that("logEvent is converted", {
  expect_migration(
    app$logEvent(msg),
    app$log_message(msg)
  )
  expect_migration_error(
    app$logEvent(msg, info),
    "`AppDriver$log_message()` does not support multiple arguments"
  )
  expect_migration_error(
    app$logEvent(),
    "`AppDriver$log_message()` requires a message argument"
  )
})


test_that("sendKeys is converted", {
  expect_migration_error(
    app$sendKeys(),
    "{shinytest2} does not support `$sendKeys()`"
  )
})


test_that("snapshotDownload is converted", {
  expect_migration(
    app$snapshotDownload("myid"),
    app$expect_download("myid")
  )
  expect_migration(
    app$snapshotDownload("myid", "myfile"),
    app$expect_download("myid", name = "myfile")
  )
})


test_that("takeScreenshot is converted", {
  expect_migration(
    app$takeScreenshot("file.png"),
    app$get_screenshot("file.png")
  )
  expect_migration(
    app$takeScreenshot("file.png", "myid"),
    app$get_screenshot("file.png", selector = "#myid")
  )
  expect_migration(
    app$takeScreenshot("file.png", myid),
    app$get_screenshot("file.png", selector = paste0("#", myid))
  )
  expect_migration(
    app$takeScreenshot(myfile, myvar),
    app$get_screenshot(myfile, selector = paste0("#", myvar))
  )
  expect_migration(
    app$takeScreenshot("file.png", "myid", parent = FALSE),
    app$get_screenshot("file.png", selector = "#myid")
  )
  expect_migration_error(
    app$takeScreenshot("file.png", "myid", parent = TRUE),
    "`ShinyDriver$takeScreenshot(parent=)` is not supported"
  )
})


test_that("takeScreenshot is converted", {
  expect_migration_error(
    app <- app$uploadFile(myid = "file.png"),
    "Use `AppDriver$get_values()` directly"
  )
  expect_migration_error(
    local({app$uploadFile(myid = "file.png")}),
    "Use `AppDriver$get_values()` directly"
  )
  expect_migration(
    app$uploadFile(myid = "file.png"),
    app$upload_file(myid = "file.png")
  )

  expect_migration(
    app$uploadFile(timeout_ = 2, myid = "file.png", wait_ = FALSE, values_ = FALSE),
    app$upload_file(myid = "file.png", wait_ = FALSE, timeout_ = 2)
  )
})


test_that("waitFor is converted", {
  expect_migration(
    app$waitForShiny(),
    app$wait_for_idle(duration = 0)
  )
})


test_that("waitForShiny is converted", {
  expect_migration(
    app$waitForShiny(),
    app$wait_for_idle(duration = 0)
  )
})


test_that("waitForValue is converted", {
  expect_migration(
    app$waitForValue(myname, ignore = foo(), timeout = 2, checkInterval = 3),
    app$wait_for_value(input = myname, ignore = foo(), timeout = 2, interval = 3)
  )
  expect_migration(
    app$waitForValue(myname, iotype = "output"),
    app$wait_for_value(output = myname)
  )
  expect_migration(
    app$waitForValue(myname, iotype = "export"),
    app$wait_for_value(export = myname)
  )
  expect_migration_error(
    app$waitForValue(myname, iotype = val),
    "must be a character value, not a variable"
  )
})


test_that("snapshotInit is converted", {
  info_env <-
    expect_migration(
      app$snapshotInit("mytest"),
      NULL,
      "`ShinyDriver$snapshotInit()` is not implemented in `AppDriver`"
    )
  expect_equal(info_env$name, "mytest")
  expect_equal(info_env$screenshot_snapshot_init, TRUE)

  suppressMessages({
    expect_migration_error(
      app$snapshotInit(myvar),
      "must be a character value, not a variable"
    )
  })
})


test_that("snapshot is converted", {
  # Typical
  expect_snapshot_migration <- function(
    ...,
    info_env = make_info_env(include_expect_screenshot = TRUE)
  ) {
    expect_migration(..., info_env = info_env)
  }
  expect_snapshot_migration(
    app$snapshot(),
    rlang::exprs(
      app$expect_values(),
      app$expect_screenshot()
    ), enexpr_new_expr = FALSE
  )
  # Use a char
  expect_snapshot_migration(
    app$snapshot(items = list(output = "myoutput")),
    rlang::exprs(
      app$expect_values(output = "myoutput"),
      app$expect_screenshot()
    ), enexpr_new_expr = FALSE
  )
  # Respect language
  expect_snapshot_migration(
    app$snapshot(items = list(output = myoutputvar)),
    rlang::exprs(
      app$expect_values(output = myoutputvar),
      app$expect_screenshot()
    ), enexpr_new_expr = FALSE
  )
  # Turn off expect_screenshot if compare_images is false
  expect_snapshot_migration(
    app$snapshot(items = list(output = myoutputvar)),
    app$expect_values(output = myoutputvar),
    info_env = make_info_env(compare_images = FALSE)
  )
  # Turn off expect_values if compare_images is TRUE
  expect_snapshot_migration(
    app$snapshot(screenshot = FALSE),
    app$expect_values(),
    info_env = make_info_env(compare_images = TRUE)
  )

  expect_migration_error(
    app$snapshot(items = a),
    "Variables may not be used"
  )
  expect_migration_error(
    app$snapshot(items = c(output = "myoutput")),
    "or a list of"
  )
  expect_migration_error(
    app$snapshot(items = TRUE),
    "is missing, `NULL`, or"
  )
})
