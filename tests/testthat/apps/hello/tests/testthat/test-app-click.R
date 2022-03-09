test_that("basic website example works", {
  app <- AppDriver$new(variant = NULL)
  app$set_inputs(name = "Hadley")
  app$set_inputs(greet = "click")
  app$expect_values(output = "greeting") # Hadley
  app$set_inputs(name = "Barret")
  app$click("greet")
  app$expect_values(output = "greeting") # Barret


  # Track clicks on `#greeting` and `#custom_div`
  app$execute_js("
    window.greeting_count = 0;

    $('#greeting').click(function () {
      window.greeting_count += 1;
    });
  ")
  app$execute_js("
    window.custom_count = 0;

    $('#custom_div').click(function () {
      window.custom_count += 1;
    });
  ")

  expect_equal(app$execute_js("return window.greeting_count"), 0)
  expect_equal(app$execute_js("return window.custom_count"), 0)

  app$click(output = "greeting")
  app$click(output = "greeting")

  expect_equal(app$execute_js("return window.greeting_count"), 2)
  expect_equal(app$execute_js("return window.custom_count"), 0)

  app$click(selector = "#custom_div")
  app$click(selector = "#custom_div")
  app$click(selector = "#custom_div")

  expect_equal(app$execute_js("return window.greeting_count"), 2)
  expect_equal(app$execute_js("return window.custom_count"), 3)
})
