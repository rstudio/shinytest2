# Comment
# app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
app <-
ShinyDriver$new(

  "../../", seed = 100, shinyOptions = list(display.mode = "normal")
)

app$snapshotInit(
  # Inner comment
  "mytest",
  screenshot = FALSE
)

# Another comment
# Only values
app$snapshot()

app$setInputs(
  bins =
8
  )
app$setInputs(bins = 9)
# Values and screenshot
app$snapshot(screenshot = TRUE)

app$setInputs(bins = 5)
app$setInputs(bins = 22)
# Only values
app$snapshot()
