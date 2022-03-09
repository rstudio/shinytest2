app_expect_unique_names <- function(self, private) {
  ckm8_assert_app_driver(self, private)

  testthat::expect_warning(
    app_check_unique_names(self, private),
    NA
  )
}


app_list_component_names <- function(self, private) {
  "!DEBUG app_list_component_names()"
  ckm8_assert_app_driver(self, private)

  res <- chromote_eval(self$get_chromote_session(), "shinytest2.listComponents()")$result$value
  lapply(res, function(vals) {
    sort_c(unlist(vals))
  })
}

app_check_unique_names <- function(self, private) {
  "!DEBUG app_check_unique_names()"
  ckm8_assert_app_driver(self, private)

  res <- app_list_component_names(self, private)

  without_other_duplicates <- function(items, other) {
    items[! items %in% other[duplicated(other)]]
  }

  has_duplicate <- function(x) {
    length(x) > 0 && any(duplicated(x))
  }
  duplicates_with_stars <- function(x) {
    ret <- unique(x[duplicated(x)])
    stats::setNames(ret, rep("*", length(ret)))
  }

  for (info in list(
    list(name = "inputs", items = res$input_not_output, question = FALSE),
    list(name = "outputs", items = res$output_not_input, question = FALSE),
    list(
      name = "inputs and outputs",
      items =
        without_other_duplicates(
          without_other_duplicates(
            res$both,
            res$input_not_output
          ),
          res$output_not_input
        ),
      question = TRUE
    )
  )) {
    if (has_duplicate(info$items)) {
      app_warn(self, private,
        c(
          "!" = paste0("Shiny ", info$name, " should have unique HTML id values."),
          "i" = "The following HTML id values are not unique:",
          duplicates_with_stars(info$items),
          if (info$question) c("i" = "Do you have a Shiny input and output value with the same name?")
        )
      )
    }
  }

  invisible(self)
}
