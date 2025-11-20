expr_recurse <- function(expr, post_fn) {
  expr_recurse_ <- function(expr, post_fn, is_top_level = FALSE) {
    if (!is.language(expr)) {
      return(expr)
    }
    expr_list <- as.list(expr)

    if (
      # Return early if it is a single item
      length(expr_list) == 1 &&
        # Make sure not something like `app$getAllValues()`
        is.language(expr_list[[1]]) &&
        length(expr_list[[1]]) == 1
    ) {
      return(expr)
    }
    for (i in seq_len(length(expr_list))) {
      val <- expr_recurse_(expr_list[[i]], post_fn, is_top_level = FALSE)
      # Handle rlang::missing() arguments
      if (rlang::is_missing(val)) {
        next
      }
      # Support the setting of `NULL` values
      expr_list[i] <- list(val)
    }

    # By being after the for-loop, it alters from the leaf to the trunk
    post_fn(expr_list, is_top_level)
  }
  # Shim `is_top_level = TRUE`
  expr_recurse_(expr = expr, post_fn = post_fn, is_top_level = TRUE)
}


st2_expr_text <- function(expr) {
  if (is.null(expr) || is.character(expr)) {
    return(expr)
  }
  if (is.list(expr)) {
    return(lapply(expr, st2_expr_text))
  }
  gsub(
    "\\s*\n    ",
    "\n  ",
    rlang::expr_text(expr, width = 60L)
  )
}
for_each_expr_text <- function(exprs, expr_fn, ...) {
  unlist(lapply(exprs, function(expr) {
    st2_expr_text(expr_fn(expr, ...))
  }))
}
