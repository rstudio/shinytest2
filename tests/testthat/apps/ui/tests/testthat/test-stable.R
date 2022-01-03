
test_that("wait for stable works", {

  app <- AppDriver$new();
  app$wait_for_stable(duration = 2 * n);

  expect_equal(app$get_values(output = "txt")$output$txt, "1 2 3")
})


test_that("waiting a lesser value will not be enough", {

  app <- AppDriver$new();
  app$wait_for_stable(duration = n / 2);

  expect_failure(
    expect_equal(app$get_values(output = "txt")$output$txt, "1 2 3")
  )
})
