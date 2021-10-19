test_that("list input & output widgets", {
  app <- ShinyDriver2$new(test_path("apps/input-widgets"))
  expect_snapshot(app$listWidgets())
})

# # TODO-barret
# test_that("warn for multiple widgets sharing an ID", {
#   expect_warning(
#     ShinyDriver2$new(test_path("apps/id-conflicts-1")),
#     "Possible duplicate input widget ids: select"
#   )

#   ## Actually apps, with duplicate output widget ids do not load currently
#   expect_error(
#     ShinyDriver2$new(test_path("apps/id-conflicts-2"), loadTimeout = 2000),
#     "Shiny app did not load"
#   )

#   expect_warning(
#     ShinyDriver2$new(test_path("apps/id-conflicts-3")),
#     "Widget ids both for input and output: widget"
#   )
# })
