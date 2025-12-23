skip_if_not_installed("shinytest")

expect_text_migration <- function(original_txt, new_txt, ...) {
  info_env <- make_info_env(..., app_var = NULL) # nolint
  suppressMessages({
    converted_text <- m__parse_test_text(
      original_txt,
      "test-migration-file",
      info_env
    )
  })
  testthat::expect_equal(converted_text, new_txt)
  info_env
}

test_that("file contents are converted", {
  info_env <- expect_text_migration(
    "
    # Comment
    1+ 1; 1 +1; 1+1;

    # Comment after blank line
    app <- ShinyDriver$new(
      \".././../\",
      seed = 100,
      loadTimeout = 10 * 1000,
      checkNames = FALSE,
      cleanLogs = TRUE,
      renderArgs = list(quiet = FALSE),
      options = list(shiny.trace = TRUE),
      # Internal comment that will be lost
      shinyOptions = list(launch.browser = FALSE)
    )
    app$snapshotInit(\"mytest\", screenshot = FALSE)

    wrapper <- function() {
      app$setInputs(bins = 4, values_ = FALSE)
    }

    # Another comment
    app$setInputs(bins = 8, values_ = FALSE)
    app$setInputs(bins = app$getAllValues()$input$bins, values_ = FALSE)
    app$snapshot()
",
    "
    # Comment
    1+ 1; 1 +1; 1+1;

    # Comment after blank line
app <- AppDriver$new(variant = paste0(\"my\", \"suffix\"), load_timeout = 10 *
  1000, check_names = FALSE, seed = 100, clean_logs = TRUE,
  shiny_args = list(launch.browser = FALSE), render_args = list(quiet = FALSE),
  options = list(shiny.trace = TRUE))

wrapper <- function() {
  app$set_inputs(bins = 4)
}

    # Another comment
app$set_inputs(bins = 8)
app$set_inputs(bins = app$get_values()$input$bins)
app$expect_values()",
    suffix = rlang::expr(paste0("my", "suffix")),
    compare_images = FALSE
  )

  expect_equal(info_env$name, "mytest")
})


test_that("`$expect_screenshot()` only shows up when `compareImages == TRUE` and `screenshot == TRUE`", {
  # No suffix, compare_images = TRUE
  expect_text_migration(
    "app <- ShinyDriver$new(\"../..\"); app$snapshotInit(\"mytest\"); app$snapshot()",
    "app <- AppDriver$new()\napp$expect_values()\napp$expect_screenshot()"
  )

  # No suffix, compare_images = TRUE w/ screenshot = FALSE; screenshot wins
  expect_text_migration(
    "app <- ShinyDriver$new(\"../..\"); app$snapshotInit(\"mytest\", screenshot = FALSE); app$snapshot()",
    "app <- AppDriver$new()\napp$expect_values()"
  )

  # No suffix, compare_images = TRUE w/ screenshot = FALSE; screenshot wins
  expect_text_migration(
    "app <- ShinyDriver$new(\"../..\"); app$snapshotInit(\"mytest\", screenshot = FALSE); app$snapshot(screenshot = TRUE)",
    "app <- AppDriver$new()\napp$expect_values()\napp$expect_screenshot()"
  )

  # Custom path, compare_images = FALSE
  expect_text_migration(
    "app <- ShinyDriver$new(\"custom/path\"); app$snapshot()",
    "app <- AppDriver$new(\"custom/path\")\napp$expect_values()",
    suffix = NULL,
    compare_images = FALSE
  )

  # compare_images = FALSE w/ screenshot = TRUE; compare_images wins
  expect_text_migration(
    "app <- ShinyDriver$new(\"../..\"); app$snapshotInit(\"mytest\", screenshot = TRUE); app$snapshot()",
    "app <- AppDriver$new()\napp$expect_values()",
    suffix = NULL,
    compare_images = FALSE
  )
})
