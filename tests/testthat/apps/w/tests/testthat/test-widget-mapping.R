test_that("input widgets", {

  app <- ShinyDriver2$new(test_path("../../."))
  expect_input_widget <- function(name, type) {
    widget <- shinytest2:::sd2_find_widget(app, NULL, name)
    expect_s3_class(widget, "Widget2")
    expect_equal(widget$get_type(), type, info = name)
    expect_equal(widget$get_iotype(), "input", info = name)
  }

  expect_input_widget("checkbox",   "checkboxInput")
  expect_input_widget("checkGroup", "checkboxGroupInput")
  expect_input_widget("date",       "dateInput")
  expect_input_widget("dates",      "dateRangeInput")
  expect_input_widget("file",       "fileInput")
  expect_input_widget("num",        "numericInput")
  expect_input_widget("radio",      "radioButtons")
  expect_input_widget("select",     "selectInput")
  expect_input_widget("slider1",    "sliderInput")
  expect_input_widget("slider2",    "sliderInput")
  expect_input_widget("text",       "textInput")

})

test_that("output widgets with the same name", {

  app <- ShinyDriver2$new(test_path("../../."))
  expect_output_widget <- function(name, type) {
    widget <- shinytest2:::sd2_find_widget(app, NULL, name, iotype = "output")
    expect_s3_class(widget, "Widget2")
    expect_equal(widget$get_type(), type, info = name)
    expect_equal(widget$get_iotype(), "output", info = name)
  }

  names <- c(
    "action_out", "checkbox_out", "checkGroup_out", "date_out", "dates_out",
    "file_out", "num_out", "radio_out", "select_out", "slider1_out",
    "slider2_out", "text_out"
  )

  for (name in names) {
    expect_output_widget(name, "verbatimTextOutput")
  }

})
