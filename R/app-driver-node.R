node_id_css_selector <- function(
  self, private,
  input = missing_arg(),
  output = missing_arg(),
  selector = missing_arg()
) {
  ckm8_assert_app_driver(self, private)

  input_provided <- !rlang::is_missing(input)
  output_provided <- !rlang::is_missing(output)
  selector_provided <- !rlang::is_missing(selector)

  if (
    sum(input_provided, output_provided, selector_provided) != 1
  ) {
    app_abort(self, private, "Must specify either `input`, `output`, or `selector`")
  }

  css_selector <-
    if (input_provided) {
      ckm8_assert_single_string(input)
      paste0("#", input, ".shiny-bound-input")
    } else if (output_provided) {
      ckm8_assert_single_string(output)
      paste0("#", output, ".shiny-bound-output")
    } else if (selector_provided) {
      ckm8_assert_single_string(selector)
      selector
    } else {
      app_abort(self, private, "Missing css type", .internal = TRUE)
    }
  css_selector

}

app_find_node_id <- function(
  self, private,
  ...,
  input = missing_arg(),
  output = missing_arg(),
  selector = missing_arg()
) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  "!DEBUG finding a nodeID"

  css_selector <- node_id_css_selector(
    self, private,
    input = input,
    output = output,
    selector = selector
  )

  el_node_ids <- chromote_find_elements(self$get_chromote_session(), css_selector)

  if (length(el_node_ids) == 0) {
    app_abort(self, private, paste0(
      "Cannot find HTML element with selector ", css_selector
    ))

  } else if (length(el_node_ids) > 1) {
    app_warn(self, private, paste0(
      "Multiple HTML elements found with selector ", css_selector
    ))
  }

  node_id <- el_node_ids[[1]]

  node_id
}


#' @importFrom rlang :=
app_click <- function(
  self, private,
  input = missing_arg(),
  output = missing_arg(),
  selector = missing_arg(),
  ...
) {
  ckm8_assert_app_driver(self, private)

  # Will validate that only a single input/output/selector was provided as a single string
  node_id <- app_find_node_id(self, private, input = input, output = output, selector = selector)

  if (!rlang::is_missing(input)) {
    # Will delay returning until outputs have been updated.
    self$set_inputs(!!input := "click", ...)

  } else {
    rlang::check_dots_empty()
    self$log_message(paste0(
      "Clicking HTML element with selector: ",
      node_id_css_selector(
        self, private,
        input = input,
        output = output,
        selector = selector
      )
    ))
    click_script <- "
      function() {
        this.click()
      }
    "

    chromote_call_js_on_node(self$get_chromote_session(), node_id, click_script)
  }


  invisible(self)
}

# # TODO-future; Not for this release. Comment for now.
# #' @description
# #' Sends the specified keys to specific HTML element. Shortcut for
# #' `find_widget()` plus `sendKeys()`.
# #' @param keys Keys to send to the widget or the app.
# # ' See [webdriver::key] for how to specific special keys.
# #' @return Self, invisibly.
# ShinyDriver2$set("public", "sendKeys", function(id, keys) {
#   "!DEBUG ShinyDriver2$sendKeys `id`"
#   private$find_widget(id)$sendKeys(keys)
#   invisible(self)
# })
