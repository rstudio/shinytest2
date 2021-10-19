test_that("input widgets", {

  app <- ShinyDriver2$new(test_path("apps/input-widgets"))

  expect_equal(app$findWidget("action")$getType(),     "actionButton")
  expect_equal(app$findWidget("checkbox")$getType(),   "checkboxInput")
  expect_equal(app$findWidget("checkGroup")$getType(), "checkboxGroupInput")
  expect_equal(app$findWidget("date")$getType(),       "dateInput")
  expect_equal(app$findWidget("dates")$getType(),      "dateRangeInput")
  expect_equal(app$findWidget("file")$getType(),       "fileInput")
  expect_equal(app$findWidget("num")$getType(),        "numericInput")
  expect_equal(app$findWidget("radio")$getType(),      "radioButtons")
  expect_equal(app$findWidget("select")$getType(),     "selectInput")
  expect_equal(app$findWidget("slider1")$getType(),    "sliderInput")
  expect_equal(app$findWidget("slider2")$getType(),    "sliderInput")
  expect_equal(app$findWidget("text")$getType(),       "textInput")

})

test_that("output widgets with the same name", {

  app <- ShinyDriver2$new(test_path("apps/input-widgets"))

  names <- c(
    "action_out", "checkbox_out", "checkGroup_out", "date_out", "dates_out",
    "file_out", "num_out", "radio_out", "select_out", "slider1_out",
    "slider2_out", "text_out"
  )

  for (n in names) {
    expect_equal(
      app$findWidget(n, "output")$getType(),
      "verbatimTextOutput",
      info = n
    )
  }

})
