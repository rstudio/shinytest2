context("app")

test_that("inputs are stored", {
  testServer(expr = {
    session$setInputs(
      integer = 593,
      decimal = 0.6,
      range = c(100, 300),
      format = 5000,
      animation = 100
    )

    # The data frame should have these properties
    df <- slider_values()
    expect_equal(nrow(df), 5)
    expect_equal(ncol(df), 2)
    expect_is(df$Value, "character")

    # Updating an input should result in a new plot
    plot1 <- output$values
    session$setInputs(integer = 594)
    plot2 <- output$values
    expect_true(plot1 != plot2)
  })
})
