
if (!fs::file_exists(
  test_path("image_diff/151-new-linux-3_6-446865a.png")
)) {
  skip("convolution test images not found")
}

test_that("convolution can be performed", {
  # Using `brew install graphicsmagick`
  # `gm compare 151-original-linux-3_6-79bf3a4.png 151-new-linux-3_6-446865a.png -highlight-style assign -file 151-diff.png`
  # `gm compare bers3-1.png bers3-2.png -highlight-style assign -file bers3-diff.png`

  # Same image
  expect_true(
    compare_screenshot_threshold(
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      threshold = NULL
    )
  )
  # Same image
  expect_true(
    compare_screenshot_threshold(
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      threshold = 0
    )
  )

  # Slightly different images
  expect_true(
    compare_screenshot_threshold(
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      test_path("image_diff/151-new-linux-3_6-446865a.png"),
      threshold = 1
    )
  )

  # Slightly different images
  expect_false(
    compare_screenshot_threshold(
      test_path("image_diff/bers3-1.png"),
      test_path("image_diff/bers3-2.png"),
      threshold = 28.1
    )
  )
  expect_true(
    compare_screenshot_threshold(
      test_path("image_diff/bers3-1.png"),
      test_path("image_diff/bers3-2.png"),
      threshold = 28.2
    )
  )

})


test_that("kernel size makes a difference", {

  small_max_diff <-
    screenshot_max_difference(
      test_path("image_diff/bers3-1.png"),
      test_path("image_diff/bers3-2.png"),
      kernel_size = 5
    )
  big_max_diff <-
    screenshot_max_difference(
      test_path("image_diff/bers3-1.png"),
      test_path("image_diff/bers3-2.png"),
      kernel_size = 10
    )

  expect_lt(small_max_diff, big_max_diff)
  expect_gt(small_max_diff, 0)
  expect_gt(big_max_diff, 44)
  expect_lt(big_max_diff, 50)
})
