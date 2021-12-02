

app_find_node_id <- function(self, private, id, iotype = c("auto", "input", "output")) {
  ckm8_assert_app_driver(self, private)

  iotype <- match.arg(iotype)
  "!DEBUG finding a nodeID by `id` and `iotype`"

  css <- if (iotype == "auto") {
    paste0("#", id)

  } else if (iotype == "input") {
    paste0("#", id, ".shiny-bound-input")

  } else if (iotype == "output") {
    paste0("#", id, ".shiny-bound-output")
  }

  el_node_ids <- chromote_find_elements(self$get_chromote_session(), css)

  if (length(el_node_ids) == 0) {
    abort(paste0(
      "Cannot find ",
      if (iotype != "auto") paste0(iotype, " "),
      "HTML element ", id
    ))

  } else if (length(el_node_ids) > 1) {
    warning(
      "Multiple ",
      if (iotype != "auto") paste0(iotype, " "),
      "HTML elements with id ", id
    )
  }

  node_id <- el_node_ids[[1]]

  node_id
}




# TODO-barret; remove code
# #' @description
# #' Finds a widget and queries its value. See the `get_value()` method of
# #' [Widget2] for more details.
# ShinyDriver2$set("public", "get_value", function(id, iotype = c("auto", "input", "output")) {
#   "!DEBUG ShinyDriver2$get_value `id` (`iotype`)"
#   private$find_widget(id, iotype)$get_value()
# })
# #' @description
# #' Finds a Shiny widget and sets its value. See the [Widget2] documentation for more details.
# #'
# #' @param value New value.
# #' @return Self, invisibly.
# ShinyDriver2$set("public", "set_value", function(id, value, iotype = c("auto", "input", "output")) {
#   "!DEBUG ShinyDriver2$set_value `id`"
#   private$find_widget(id, iotype)$set_value(value)
#   invisible(self)
# })

app_click <- function(self, private, id, iotype = c("auto", "input", "output")) {
  ckm8_assert_app_driver(self, private)

  node_id <- app_find_node_id(self, private, id = id, iotype = iotype)
  click_script <- "
    function() {
      var el = $(this);
      el.click();
    }
  "
  # TODO-barret; Change to `this.click()`?
  # TODO-barret; Update the docs to reflect this change.
  # Ex: document.getElementById("myCheck").click()

  chromote_call_js_on_node(self$get_chromote_session(), node_id, click_script)

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


app_list_component_names <- function(self, private) {
  "!DEBUG app_list_component_names()"
  ckm8_assert_app_driver(self, private)

  res <- chromote_eval(self$get_chromote_session(), "shinytest2.listComponents()")$result$value

  res$input <- sort_c(unlist(res$input))
  res$output <- sort_c(unlist(res$output))
  res
}

app_check_unique_widget_names <- function(self, private) {
  "!DEBUG app_check_unique_widget_names()"
  ckm8_assert_app_driver(self, private)

  names <- app_list_component_names(self, private)
  inputs <- names$input
  outputs <- names$output

  check <- function(what, ids) {
    if (any(duplicated(ids))) {
      dup <- paste(unique(ids[duplicated(ids)]), collapse = ", ")
      warning("Possible duplicate ", what, " widget ids: ", dup)
    }
  }

  if (any(inputs %in% outputs)) {
    dups <- unique(inputs[inputs %in% outputs])
    warning(
      "Widget ids both for input and output: ",
      paste(dups, collapse = ", ")
    )

    ## Otherwise the following checks report it, too
    inputs <- setdiff(inputs, dups)
    outputs <- setdiff(outputs, dups)
  }

  if (length(inputs) > 0) check("input", inputs)
  if (length(outputs) > 0) check("output", outputs)

  invisible(self)
}
