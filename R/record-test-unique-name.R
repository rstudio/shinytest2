# Also used in recorder
known_app_driver_name_values <- function(test_file) {

  exprs <- parse(file = test_file)

  is_appdriver_new <- function(expr_list) {
    length(expr_list) >= 1 &&
      is.language(expr_list[[1]]) &&
      st2_expr_text(expr_list[[1]]) %in% c(
        "AppDriver$new",
        "shinytest2::AppDriver$new",
        "shinytest2:::AppDriver$new"
      )
  }

  known_names <- list()
  post_fn <- function(expr_list, is_top_level) {
    if (is_appdriver_new(expr_list)) {
      args <- rlang::call_args(
        rlang::call_match(
          as.call(expr_list),
          AppDriver$public_methods$initialize
        )
      )

      new_name <- args$name
      if (
        is.null(new_name) || # allow `NULL`
        rlang::is_scalar_character(new_name) # allow single character value
        # Do not allow symbols or language expressions
      ) {
        # Allows the collecting of NULL name values
        known_names[length(known_names) + 1] <<- list(new_name)
      }
    }
    # Don't alter the expr_list, just return it
    as.call(expr_list)
  }
  # For all exprs, find a single shinytest::testApp()
  lapply(exprs, function(expr) {
    expr_recurse(expr, post_fn = post_fn)
  })

  known_names
}
