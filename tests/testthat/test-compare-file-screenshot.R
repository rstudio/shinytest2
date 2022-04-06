
test_that("Screenshot comparison allows for tolerance", {
  skip_if_not_installed("png")

  make_fake_image <- function(height = 5, width = height) {
    # Make random data for three different channels
    dt <- array(runif(3 * height * width), c(height, width, 3))
    # Make sure the data is in the set [0, 255] / 255
    floor(dt * 255) / 255
  }

  img1 <- make_fake_image()

  img2 <- img1 * 255
  img3 <- img1 * 255
  for (i in 1:3) {
    # adjust 1 pixel
    extra <- if (img2[1, 1, i] == 255) -1 else 1
    message("img1[1,1, i] * 255: ", img2[1, 1, i])
    message("img2 extra: ", extra)
    img2[1, 1, i] <- img2[1, 1, i] + extra

    # adjust 2 pixels
    extra <- if (img3[1, 1, i] >= 254) -2 else 2
    message("img3 extra: ", extra)
    img3[1, 1, i] <- img3[1, 1, i] + extra
  }
  img2 <- img2 / 255
  img3 <- img3 / 255

  # Save images
  img1_file <- tempfile(fileext = ".png")
  img2_file <- tempfile(fileext = ".png")
  img3_file <- tempfile(fileext = ".png")
  withr::defer({
    unlink(c(
      img1_file,
      img2_file,
      img3_file
    ))
  })
  png::writePNG(img1, img1_file)
  png::writePNG(img2, img2_file)
  png::writePNG(img3, img3_file)


  expect_equal(
    compare_file_screenshot(img1_file, img1_file, tolerance = 0),
    TRUE,
    info = "Same images are the same"
  )

  expect_equal(
    compare_file_screenshot(img1_file, img2_file, tolerance = 1),
    TRUE,
    info = "Off by 1 is OK"
  )
  expect_equal(
    compare_file_screenshot(img1_file, img2_file, tolerance = 0),
    FALSE,
    info = "Off by 1 with no tolerance is not OK"
  )

  expect_equal(
    compare_file_screenshot(img1_file, img3_file, tolerance = 1),
    FALSE,
    info = "Off by 2 with tolerance of 1 is not OK"
  )
  expect_equal(
    compare_file_screenshot(img1_file, img3_file, tolerance = 2),
    TRUE,
    info = "Off by 2 with tolerance of 2 is OK"
  )

  browser()
})
