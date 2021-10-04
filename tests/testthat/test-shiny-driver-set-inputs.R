# Current shinytest2 code using `app$**()`:
test_that("set kitchen sink of inputs", {
  skip_if_not_installed("shinyWidgets")
  app <- ShinyDriver2$new(test_path("apps/input-widgets"), variant = os_name_and_r_version())

  app$expectSnapshot(screenshot = TRUE)

  app$setInputs(
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
    # "file" = null,
    "select" = "2",
    "slider1" = 65,
    "slider2" = c(35,85),
    "text" = "Text entered",
  )
  app$expectSnapshot(screenshot = TRUE)

  app$setInputs(tabset = "shinyWidgets")

  app$expectSnapshot(screenshot = TRUE)

  app$setInputs(
    "bsSwitch" = TRUE,
    "matSwitch" = TRUE,
    "picker" = c("T", "E", "S"),
    "prettyCheckbox" = TRUE,
  )
  #     "checkboxGroupButtons" = c(2,3),
  #     "knob" = 50,
  #     "search" = "Test text", # must hit enter to submit
  #     "sliderText" = "Strongly agree",
  # )

  app$expectSnapshot(screenshot = TRUE)
  # browser()
})
