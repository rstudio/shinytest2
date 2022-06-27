
if (!fs::file_exists(test_path("image_diff/151-new-linux-3_6-446865a.png"))) {
  skip("convolution test images not found")
}

test_that("convolution can be performed", {

  # Using `brew install graphicsmagick`
  # `gm compare 151-original-linux-3_6-79bf3a4.png 151-new-linux-3_6-446865a.png -highlight-style assign -file 151-diff.png`

  # Same image
  expect_true(
    compare_file_screenshot(
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      tolerance = NULL
    )
  )
  # Same image
  expect_true(
    compare_file_screenshot(
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      tolerance = 0
    )
  )

  # Slightly different images
  expect_true(
    compare_file_screenshot(
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      test_path("image_diff/151-new-linux-3_6-446865a.png"),
      tolerance = 1
    )
  )

  # Slightly different images
  expect_false(
    compare_file_screenshot(
      test_path("image_diff/151-original-linux-3_6-79bf3a4.png"),
      test_path("image_diff/151-new-linux-3_6-446865a.png"),
      tolerance = 20
    )
  )

})
