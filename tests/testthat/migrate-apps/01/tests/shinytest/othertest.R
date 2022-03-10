# Comment
# app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
app <- ShinyDriver$new(

  "../../", seed = 100, shinyOptions = list(display.mode = "normal")
)

# expect_pass(shinytest::testApp("../", suffix = osName(), compareImages = TRUE))

app$snapshotInit(
  # Inner comment
  "barret",
  screenshot = FALSE
)

# Another comment
app$snapshot(screenshot = FALSE)

app$setInputs(
  bins =
8
  )
app$setInputs(bins = 9)
app$snapshot(screenshot = TRUE)

app$setInputs(bins = 5)
app$setInputs(bins = 22)
app$snapshot()
