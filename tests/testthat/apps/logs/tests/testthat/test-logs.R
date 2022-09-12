## Trouble finding `testthat::expect_match()
# nolint start

expect_log_tests <- function(log) {
  # print(log)
  has_websocket <- any(grepl("websocket", log, fixed = TRUE))
  msg <- function(txt) {
    if (has_websocket) {
      # "{shiny}      R  info  ----------- Cat msg!"
      # to
      # "{shiny}      R  info     ----------- Cat msg!"
      txt <- sub("(info  |error |throw |log   |              )", "\\1    ", txt)
    }
    txt
  }
  expect_match(log, msg("{shiny}      R  info  ----------- Cat msg!"), all = FALSE, fixed = TRUE)
  expect_match(log, msg("{shiny}      R  error ----------- Message msg!"), all = FALSE, fixed = TRUE)

  expect_match(log, msg("\\{shinytest2\\} R  info  \\d\\d:\\d\\d:\\d\\d.\\d\\d Starting Shiny app"), all = FALSE)
  expect_match(log, msg("\\{chromote\\}   JS info  \\d\\d:\\d\\d:\\d\\d.\\d\\d shinytest2; Loaded"), all = FALSE)

  expect_match(log, msg("\\{chromote\\}   JS throw \\d\\d:\\d\\d:\\d\\d.\\d\\d Uncaught Exception msg"), all = FALSE)
  expect_match(log, msg("\\{chromote\\}   JS throw \\d\\d:\\d\\d:\\d\\d.\\d\\d Uncaught TypeError: window.test_method is not a function"), all = FALSE)

  for (extra_msg in c(
    "Nullish null undefined",
    "Boolean false true",
    "Character abc",
    "Number 123",
    "BigInt 10n",
    "Object \\[object Object\\]",
    "Math \\[object Math\\]",
    "Symbol Symbol\\(abc\\)",
    "Array \\[1,2,3\\]",
    "Function function Date\\(\\)",
    NULL
  )) {
    expect_match(
      log,
      msg(paste0("\\{chromote\\}   JS log   \\d\\d:\\d\\d:\\d\\d.\\d\\d ", extra_msg)),
      all = FALSE
    )
  }

  expect_match(
    log[which(grepl("Character abc", log)) + 1],
    msg("                                  (anonymous) @ "),
    all = FALSE, fixed = TRUE
  )
  expect_match(
    log[which(grepl("Uncaught Exception msg", log)) + 1],
    msg("                                  (anonymous) @ "),
    all = FALSE,
    fixed = TRUE
  )
}

test_that("App captures known debug messages", {
  app <- AppDriver$new()
  withr::defer(app$stop())

  log_df <- app$get_logs()

  expect_s3_class(log_df, "shinytest2_log")
  checkmate::assert_names(
    names(log_df),
    identical.to = c("workerid", "timestamp", "location", "level", "message")
  )

  log <- strsplit(format(log_df), "\n")[[1]]
  # > print(log)
  #  [1] "{shinytest2} R  info  15:08:43.50 Start AppDriver initialization"
  #  [2] "{shinytest2} R  info  15:08:43.50 Starting Shiny app"
  #  [3] "{shinytest2} R  info  15:08:44.15 Creating new chromote session"
  #  [4] "{shinytest2} R  info  15:08:44.28 Navigating to Shiny app"
  #  [5] "{shinytest2} R  info  15:08:44.34 Injecting shiny-tracer.js"
  #  [6] "{chromote}   JS info  15:08:44.35 shinytest2; Loaded"
  #  [7] "{chromote}   JS info  15:08:44.35 shinytest2; jQuery not found"
  #  [8] "{shinytest2} R  info  15:08:44.35 Waiting until Shiny app starts"
  #  [9] "{chromote}   JS log   15:08:44.41 Log msg"
  # [10] "                                  (anonymous) @ :19:20"
  # [11] "{chromote}   JS info  15:08:44.43 shinytest2; Waiting for shiny session to connect"
  # [12] "{chromote}   JS throw 15:08:44.43 Uncaught Exception msg"
  # [13] "                                  (anonymous) @ :20:36"
  # [16] "{chromote}   JS info  15:08:44.53 shinytest2; Connected"
  # [17] "{chromote}   JS info  15:08:44.53 shinytest2; Ready"
  # [20] "{shinytest2} R  info  15:08:44.58 Shiny app started"
  # [21] "{shiny}      R  info  ----------- Cat msg!"
  # [22] "{shiny}      R  error ----------- Loading required package: shiny"
  # [23] "{shiny}      R  error ----------- Message msg!"
  # [24] "{shiny}      R  error ----------- Running application in test mode."
  # [25] "{shiny}      R  error ----------- "
  # [26] "{shiny}      R  error ----------- Listening on http://127.0.0.1:9372"


  expect_log_tests(log)

  # validate that no websocket traffic exists
  expect_failure(
    expect_match(log, "websocket", all = FALSE, fixed = TRUE)
  )
})


test_that("App captures known debug messages", {
  app <- AppDriver$new(options = list(shiny.trace = TRUE))
  withr::defer(app$stop())

  log_df <- app$get_logs()

  expect_s3_class(log_df, "shinytest2_log")
  checkmate::assert_names(
    names(log_df),
    identical.to = c("workerid", "timestamp", "location", "level", "message")
  )

  log <- strsplit(format(log_df), "\n")[[1]]
  # > print(log)
  #  [1] "{shinytest2} R  info      09:38:32.87 Start AppDriver initialization"
  #  [2] "{shinytest2} R  info      09:38:32.87 Starting Shiny app"
  #  [3] "{shinytest2} R  info      09:38:33.52 Creating new chromote session"
  #  [4] "{shinytest2} R  info      09:38:33.66 Navigating to Shiny app"
  #  [5] "{shinytest2} R  info      09:38:33.73 Injecting shiny-tracer.js"
  #  [6] "{chromote}   JS info      09:38:33.73 shinytest2; Loaded"
  #  [7] "{chromote}   JS info      09:38:33.75 shinytest2; jQuery not found"
  #  [8] "{shinytest2} R  info      09:38:33.77 Waiting until Shiny app starts"
  #  [9] "{chromote}   JS log       09:38:33.79 Log msg"
  # [10] "                                      (anonymous) @ :20:20"
  # [11] "{chromote}   JS info      09:38:33.82 shinytest2; Waiting for shiny session to connect"
  # [12] "{chromote}   JS throw     09:38:33.82 Uncaught Exception msg"
  # [13] "                                      (anonymous) @ :21:36"
  # [14] "{chromote}   JS websocket 09:38:33.83 send {\"method\":\"init\",\"data\":{\".clientdata_output_time_hidden\":false,\".clientdata_pixelratio\":1,\".clientdata_url_protocol\":\"http:\",\".clientdata_url_hostname\":\"127.0.0.1\",\".clientdata_url_port\":\"25358\",\".clientdata_url_pathname\":\"/\",\".clientdata_url_search\":\"\",\".clientdata_url_hash_initial\":\"\",\".clientdata_url_hash\":\"\",\".clientdata_singletons\":\"\"}}"
  # [15] "{chromote}   JS websocket 09:38:33.92 recv {\"config\":{\"workerId\":\"\",\"sessionId\":\"ebd61a312f870c029d195506a256385a\",\"user\":null}}"
  # [16] "{chromote}   JS info      09:38:33.92 shinytest2; Connected"
  # [17] "{chromote}   JS info      09:38:33.92 shinytest2; Ready"
  # [18] "{chromote}   JS websocket 09:38:33.97 recv {\"busy\":\"busy\"}"
  # [19] "{chromote}   JS info      09:38:33.97 shinytest2; shiny:busy"
  # [20] "{chromote}   JS websocket 09:38:33.99 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [21] "{chromote}   JS websocket 09:38:33.99 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [22] "{chromote}   JS websocket 09:38:34.00 recv {\"busy\":\"idle\"}"
  # [23] "{chromote}   JS info      09:38:34.00 shinytest2; shiny:idle"
  # [24] "{chromote}   JS websocket 09:38:34.00 recv {\"errors\":{},\"values\":{\"time\":\"1640097514\"},\"inputMessages\":[]}"
  # [25] "{chromote}   JS info      09:38:34.00 shinytest2; shiny:value time"
  # [26] "{shinytest2} R  info      09:38:34.02 Shiny app started"
  # [27] "{chromote}   JS websocket 09:38:36.99 recv {\"progress\":{\"type\":\"binding\",\"message\":{\"id\":\"time\"}}}"
  # [28] "{chromote}   JS websocket 09:38:37.00 recv {\"busy\":\"busy\"}"
  # [29] "{chromote}   JS info      09:38:37.00 shinytest2; shiny:busy"
  # [30] "{chromote}   JS websocket 09:38:37.02 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [31] "{chromote}   JS websocket 09:38:37.02 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [32] "{chromote}   JS websocket 09:38:37.03 recv {\"busy\":\"idle\"}"
  # [33] "{chromote}   JS info      09:38:37.03 shinytest2; shiny:idle"
  # [34] "{chromote}   JS websocket 09:38:37.06 recv {\"errors\":{},\"values\":{\"time\":\"1640097517\"},\"inputMessages\":[]}"
  # [35] "{chromote}   JS info      09:38:37.06 shinytest2; shiny:value time"
  # [36] "{chromote}   JS websocket 09:38:40.03 recv {\"progress\":{\"type\":\"binding\",\"message\":{\"id\":\"time\"}}}"
  # [37] "{chromote}   JS websocket 09:38:40.03 recv {\"busy\":\"busy\"}"
  # [38] "{chromote}   JS info      09:38:40.03 shinytest2; shiny:busy"
  # [39] "{chromote}   JS websocket 09:38:40.03 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [40] "{chromote}   JS websocket 09:38:40.03 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [41] "{chromote}   JS websocket 09:38:40.03 recv {\"busy\":\"idle\"}"
  # [42] "{chromote}   JS info      09:38:40.03 shinytest2; shiny:idle"
  # [43] "{chromote}   JS websocket 09:38:40.04 recv {\"errors\":{},\"values\":{\"time\":\"1640097520\"},\"inputMessages\":[]}"
  # [44] "{chromote}   JS info      09:38:40.04 shinytest2; shiny:value time"
  # [45] "{chromote}   JS websocket 09:38:43.04 recv {\"progress\":{\"type\":\"binding\",\"message\":{\"id\":\"time\"}}}"
  # [46] "{chromote}   JS websocket 09:38:43.04 recv {\"busy\":\"busy\"}"
  # [47] "{chromote}   JS info      09:38:43.04 shinytest2; shiny:busy"
  # [48] "{chromote}   JS websocket 09:38:43.04 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [49] "{chromote}   JS websocket 09:38:43.04 recv {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [50] "{chromote}   JS websocket 09:38:43.04 recv {\"busy\":\"idle\"}"
  # [51] "{chromote}   JS info      09:38:43.04 shinytest2; shinyidle"
  # [52] "{chromote}   JS websocket 09:38:43.04 recv {\"errors\":{},\"values\":{\"time\":\"1640097523\"},\"inputMessages\":[]}"
  # [53] "{chromote}   JS info      09:38:43.04 shinytest2; shiny:value time"
  # [54] "{shiny}      R  info      ----------- Cat msg!"
  # [55] "{shiny}      R  error     ----------- Loading required package: shiny"
  # [56] "{shiny}      R  error     ----------- Message msg!"
  # [57] "{shiny}      R  error     ----------- Running application in test mode."
  # [58] "{shiny}      R  error     ----------- "
  # [59] "{shiny}      R  error     ----------- Listening on http://127.0.0.1:25358"
  # [60] "{shiny}      R  error     ----------- SEND {\"config\":{\"workerId\":\"\",\"sessionId\":\"ebd61a312f870c029d195506a256385a\",\"user\":null}}"
  # [61] "{shiny}      R  error     ----------- RECV {\"method\":\"init\",\"data\":{\".clientdata_output_time_hidden\":false,\".clientdata_pixelratio\":1,\".clientdata_url_protocol\":\"http:\",\".clientdata_url_hostname\":\"127.0.0.1\",\".clientdata_url_port\":\"25358\",\".clientdata_url_pathname\":\"/\",\".clientdata_url_search\":\"\",\".clientdata_url_hash_initial\":\"\",\".clientdata_url_hash\":\"\",\".clientdata_singletons\":\"\"}}"
  # [62] "{shiny}      R  error     ----------- SEND {\"busy\":\"busy\"}"
  # [63] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [64] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [65] "{shiny}      R  error     ----------- SEND {\"busy\":\"idle\"}"
  # [66] "{shiny}      R  error     ----------- SEND {\"errors\":{},\"values\":{\"time\":\"1640097514\"},\"inputMessages\":[]}"
  # [67] "{shiny}      R  error     ----------- SEND {\"progress\":{\"type\":\"binding\",\"message\":{\"id\":\"time\"}}}"
  # [68] "{shiny}      R  error     ----------- SEND {\"busy\":\"busy\"}"
  # [69] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [70] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [71] "{shiny}      R  error     ----------- SEND {\"busy\":\"idle\"}"
  # [72] "{shiny}      R  error     ----------- SEND {\"errors\":{},\"values\":{\"time\":\"1640097517\"},\"inputMessages\":[]}"
  # [73] "{shiny}      R  error     ----------- SEND {\"progress\":{\"type\":\"binding\",\"message\":{\"id\":\"time\"}}}"
  # [74] "{shiny}      R  error     ----------- SEND {\"busy\":\"busy\"}"
  # [75] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [76] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [77] "{shiny}      R  error     ----------- SEND {\"busy\":\"idle\"}"
  # [78] "{shiny}      R  error     ----------- SEND {\"errors\":{},\"values\":{\"time\":\"1640097520\"},\"inputMessages\":[]}"
  # [79] "{shiny}      R  error     ----------- SEND {\"progress\":{\"type\":\"binding\",\"message\":{\"id\":\"time\"}}}"
  # [80] "{shiny}      R  error     ----------- SEND {\"busy\":\"busy\"}"
  # [81] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculating\"}}"
  # [82] "{shiny}      R  error     ----------- SEND {\"recalculating\":{\"name\":\"time\",\"status\":\"recalculated\"}}"
  # [83] "{shiny}      R  error     ----------- SEND {\"busy\":\"idle\"}"
  # [84] "{shiny}      R  error     ----------- SEND {\"errors\":{},\"values\":{\"time\":\"1640097523\"},\"inputMessages\":[]}"

  expect_log_tests(log)

  # check that websocket traffic exists
  expect_match(log, "websocket", all = FALSE, fixed = TRUE)

  expect_match(log, "----------- SEND {\"busy\":\"busy\"}", all = FALSE, fixed = TRUE)
  expect_match(log, "\\{chromote\\}   JS websocket \\d\\d:\\d\\d:\\d\\d.\\d\\d recv \\{\"busy\":\"busy\"\\}", all = FALSE)

})

# nolint end
