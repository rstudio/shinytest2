app <- ShinyDriver$new("../../", seed = 100, shinyOptions = list(display.mode = "normal"))
app$snapshotInit("mytest")

app$uploadFile(file1 = "Rock.csv")
app$snapshot()
app$uploadFile(file1 = "other/Rock.csv")
app$snapshot()
