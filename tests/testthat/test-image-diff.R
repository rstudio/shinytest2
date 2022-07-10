
img_folder <- system.file("example/imgs", package = "shinytest2")
bookmark_new <- fs::path(img_folder, "bookmark-new.png")
bookmark_old <- fs::path(img_folder, "bookmark-old.png")

slider_new <- fs::path(img_folder, "slider-new.png")
slider_old <- fs::path(img_folder, "slider-old.png")

test_that("convolution can be performed", {
  # Using `brew install graphicsmagick`
  # `gm compare 151-original-linux-3_6-79bf3a4.png 151-new-linux-3_6-446865a.png -highlight-style assign -file 151-diff.png`
  # `gm compare bers3-1.png bers3-2.png -highlight-style assign -file bers3-diff.png`

  # Same image
  expect_true(
    compare_screenshot_threshold(
      bookmark_old,
      bookmark_old,
      threshold = NULL
    )
  )
  # Same image
  expect_true(
    compare_screenshot_threshold(
      bookmark_old,
      bookmark_old,
      threshold = 0
    )
  )

  # Slightly different images
  expect_true(
    compare_screenshot_threshold(
      bookmark_old,
      bookmark_new,
      threshold = 1
    )
  )


  # Slightly different images
  expect_false(
    compare_screenshot_threshold(
      slider_old,
      slider_new,
      threshold = 28.1,
      quiet = TRUE
    )
  )

  expect_silent({
    ans <-
      compare_screenshot_threshold(
        slider_old,
        slider_new,
        threshold = 28.2
      )
  })
  expect_true(ans)

})


test_that("kernel size makes a difference", {

  small_max_diff <-
    screenshot_max_difference(
      slider_old,
      slider_new,
      kernel_size = 5
    )
  big_max_diff <-
    screenshot_max_difference(
      slider_old,
      slider_new,
      kernel_size = 10
    )

  expect_lt(small_max_diff, big_max_diff)
  expect_gt(small_max_diff, 0)
  expect_gt(big_max_diff, 44)
  expect_lt(big_max_diff, 50)
})
