


test_that("App captures known debug messages", {
  app <- AppDriver$new(test_path("../../."))

  log_df <- app$get_debug_log()
  log <- strsplit(format(log_df), "\n")[[1]]
  # > print(log)
  #  [1] "{shinytest2} R  info  15:08:43.50 Start AppDriver initialization"
  #  [2] "{shinytest2} R  info  15:08:43.50 Starting Shiny app"
  #  [3] "{shinytest2} R  info  15:08:44.15 Creating new chromote session"
  #  [4] "{shinytest2} R  info  15:08:44.28 Navigating to Shiny app"
  #  [5] "{shinytest2} R  info  15:08:44.34 Injecting shiny-tracer.js"
  #  [6] "{chromote}   JS info  15:08:44.35 shinytest2: window.shinytest2 loaded"
  #  [7] "{chromote}   JS info  15:08:44.35 shinytest2: jQuery not loaded yet"
  #  [8] "{shinytest2} R  info  15:08:44.35 Waiting until Shiny app starts"
  #  [9] "{chromote}   JS log   15:08:44.41 Log msg"
  # [10] "                                  (anonymous) @ :19:20"
  # [11] "{chromote}   JS info  15:08:44.43 shinytest2: waiting for shiny session to connect"
  # [12] "{chromote}   JS throw 15:08:44.43 Uncaught Exception msg"
  # [13] "                                  (anonymous) @ :20:36"
  # [16] "{chromote}   JS info  15:08:44.53 shinytest2: connected"
  # [17] "{chromote}   JS info  15:08:44.53 shinytest2: ready"
  # [20] "{shinytest2} R  info  15:08:44.58 Shiny app started"
  # [21] "{shiny}      R  info  ----------- Cat msg!"
  # [22] "{shiny}      R  error ----------- Loading required package: shiny"
  # [23] "{shiny}      R  error ----------- Message msg!"
  # [24] "{shiny}      R  error ----------- Running application in test mode."
  # [25] "{shiny}      R  error ----------- "
  # [26] "{shiny}      R  error ----------- Listening on http://127.0.0.1:9372"

  expect_true(any(log == "{shiny}      R  info  ----------- Cat msg!"))
  expect_true(any(log == "{shiny}      R  error ----------- Message msg!"))

  expect_true(any(grepl("\\{shinytest2\\} R  info  \\d\\d:\\d\\d:\\d\\d.\\d\\d Starting Shiny app", log)))

  expect_true(any(grepl("\\{chromote\\}   JS info  \\d\\d:\\d\\d:\\d\\d.\\d\\d shinytest2: window.shinytest2 loaded", log)))

  expect_true(any(grepl("\\{chromote\\}   JS throw \\d\\d:\\d\\d:\\d\\d.\\d\\d Uncaught Exception msg", log)))
  expect_true(any(grepl("\\{chromote\\}   JS log   \\d\\d:\\d\\d:\\d\\d.\\d\\d Log msg", log)))
  expect_true(grepl("                                  (anonymous) @ ", log[which(grepl("Log msg", log)) + 1], fixed = TRUE))
  expect_true(grepl("                                  (anonymous) @ ", log[which(grepl("Uncaught Exception msg", log)) + 1], fixed = TRUE))
})
