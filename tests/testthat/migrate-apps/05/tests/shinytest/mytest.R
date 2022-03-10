app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
app$snapshotInit("mytest")

app$setInputs(decimal = 1)
app$snapshot()
app$setInputs(range = c(200, 1000))
app$setInputs(format = 7500)
app$setInputs(animation = 1051)
app$snapshot()
