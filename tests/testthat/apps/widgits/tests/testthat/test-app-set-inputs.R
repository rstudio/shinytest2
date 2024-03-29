# Current shinytest2 code using `app$**()`:
test_that("set kitchen sink of inputs", {
  skip_if_not_installed("shinyWidgets")

  app <- AppDriver$new(
    # variant = platform_variant()
    variant = NULL
  )

  app$expect_values()
  # app$expect_screenshot()

  app$set_inputs(
    "action" = "click",
    "checkbox" = TRUE,
    "checkGroup" = c("2", "3"),
    "date" = "2015-01-01",
    "dates" = c(
      "2015-01-01",
      "2015-02-20"
    ),
    "num" = 100,
    "radio" = "2",
    # "file" = NULL,
    "select" = "2",
    "slider1" = 65,
    "slider2" = c(35, 85),
    "text" = "Text entered",
  )
  # File upload
  local({
    tmpfile <- "__tmpfile.txt"
    cat("tmpfile content", file = tmpfile)
    withr::defer(unlink(tmpfile))
    app$upload_file(file = tmpfile)
  })

  app$expect_values()
  # app$expect_screenshot()

  app$set_inputs(tabset = "shinyWidgets")

  app$expect_values()
  # app$expect_screenshot()

  app$set_inputs(
    "bsSwitch" = TRUE,
    "matSwitch" = TRUE,
    "picker" = c("T", "E", "S"),
    "prettyCheckbox" = TRUE,
  )

  # TODO-future; Currently do not work
  #     "checkboxGroupButtons" = c(2,3),
  #     "knob" = 50,
  #     "search" = "Test text", # must hit enter to submit
  #     "sliderText" = "Strongly agree",

  Sys.sleep(0.5) # css animations
  app$expect_values()
  # app$expect_screenshot()
})
