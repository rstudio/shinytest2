# Current shinytest2 code using `app$**()`:
test_that("set kitchen sink of inputs", {
  skip_if_not_installed("shinyWidgets")

  app <- ShinyDriver2$new(test_path("../../."), variant = os_name_and_r_version())

  app_expect_appshot(app, screenshot = TRUE)

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
  tmpfile <- "__tmpfile"
  cat("tmpfile content", file = tmpfile)
  on.exit({unlink(tmpfile)}, add = TRUE)
  app$uploadFile(file = test_path("test-app-set-inputs.R"))

  app_expect_appshot(app, screenshot = TRUE)

  app$set_inputs(tabset = "shinyWidgets")

  app_expect_appshot(app, screenshot = TRUE)

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

  app_expect_appshot(app, screenshot = TRUE)
})
