migrate <- function(path, quiet = FALSE) {

  path_info <- app_path(path, "path")

  if (TRUE) {
    new_path <- fs::path(fs::path_dir(path_info$dir), paste0(fs::path_file(path_info$dir), "-tmp"))
    if (fs::dir_exists(new_path)) fs::dir_delete(new_path)
    fs::dir_copy(path_info$dir, new_path)
    path_info <- app_path(new_path, "path")
  }

  # Use an environment to avoid having to return many function levels to merge info and then send back many function levels
  app_info_env <- as.environment(path_info)
  app_info_env$verbose <- !isTRUE(quiet)

  withr::with_dir(path_info$dir, {
    migrate__extract_runner_info(app_info_env)
    migrate__write_shinytest2_runner(app_info_env)

    migrate__parse_test_files(app_info_env)

  })
}

# Make sure all folders exist as expected
# @return shinytest runner file location
migrate__validate_shinytest_exists <- function() {
  if (!fs::dir_exists("tests")) abort("No ./tests directory found")
  shinytest_file <- fs::dir_ls("tests", regexp = "shinytest\\.[rR]$", type = "file")
  if (length(shinytest_file) == 0) abort("No ./tests/shinytest.R file found")
  if (length(shinytest_file) > 1) abort("Multiple files matching `./tests/shinytest` found")
  if (!fs::dir_exists("./tests/shinytest")) abort("./tests/shinytest folder not found")

  # return runner file location
  unname(shinytest_file)
}


# Extract the runner information, such as `suffix` and a fully populated `testnames`
migrate__extract_runner_info <- function(app_info_env) {
  shinytest_file <- migrate__validate_shinytest_exists()
  exprs <- parse(file = shinytest_file)
  file_env <- new.env(parent = globalenv())

  for (expr in exprs) {

    expr_fn <- as.character(expr[[1]])
    if (expr_fn == "expect_pass") {
      expr <- expr[[2]]
      expr_fn <- as.character(expr[[1]])
    }
    if (expr_fn[[1]] %in% c("::", ":::")) {
      if (expr_fn[[2]] != "shinytest") next
      expr_fn <- expr_fn[[3]]
    }
    if (expr_fn != "testApp") next

    # Is `testApp` function!

    # Get standardised arguments
    test_call <- rlang::call_standardise(expr)
    test_call_app_dir <- test_call$appDir
    if (fs::path_rel(test_call_app_dir) != "..") {
      abort(paste0(
        "shinytest::testApp() must be called on the parent directory.\n",
        "{shinytest2} does not know how to automatically migrate this app."
      ))
    }
    # Store knowledge
    app_info_env$test_names <-
      shinytest___find_tests("tests/shinytest", test_call$testnames %||% NULL)
    app_info_env$suffix <- test_call$suffix %||% NULL
    app_info_env$compare_images <- test_call$compareImages %||% TRUE
    app_info_env$call <- test_call

    # Nothing left to find. Quit early
    return()
  }
}

# Save a new shinytest2 runner file
migrate__write_shinytest2_runner <- function(app_info_env) {
  if (app_info_env$verbose) {
    if (file.exists("tests/testthat.R")) {
      rlang::inform(c("!" = "Overwriting `tests/testthat.R` with a {shinytest2} test runner"))
    } else {
      rlang::inform(c("*" = "Writing a {shinytest2} based test runner: `tests/testthat.R`"))
    }
  }
  fs::file_copy(
    system.file("internal/template/testthat.R", package = "shinytest2"),
    "tests/testthat.R",
    overwrite = TRUE
  )
}


migrate__parse_test_files <- function(app_info_env) {
  # test_names <- app_info_env$test_names
  # suffix <- app_info_env$suffix
  # compare_images <- app_info_env$compare_images
  # call <- app_info_env$call
  if (app_info_env$verbose) {
    rlang::inform(c("i" = paste0("`suffix` found: ", app_info_env$suffix)))
    rlang::inform(c("i" = paste0("`testnames` found: ", paste0(app_info_env$test_names, collapse = ", "))))
  }

  lapply(app_info_env$test_names, function(test_name) {
    test_path <- file.path("tests/shinytest", test_name)
    migrate__parse_test_file(test_path, app_info_env)
  })

}

migrate__parse_test_file <- function(test_path, app_info_env) {
  if (app_info_env$verbose) {
    rlang::inform(c("i" = paste0("Migrating test", test_path)))
  }

  info_env <- new.env(parent = app_info_env)
  info_env$test_path <- test_path
  info_env$save_path <- fs::path("tests", "testthat", paste0("test-", fs::path_file(test_path)))

  test_exprs <- parse(file = test_path)
  test_text <- read_utf8(test_path)
  test_lines <- strsplit(test_text, "\n")[[1]]


  if (length(test_lines) == 0) {
    if (app_info_env$verbose) {
      rlang::inform(paste0("No test content found in `{test_path}`"))
      rlang::inform(paste0("Creating: ", info_env$save_path))
    }
    file.copy(test_path, info_env$save_path)
    return()
  }

  init_line_pos <- which(grepl("ShinyDriver$new(", test_lines, fixed = TRUE))
  if (length(init_line_pos) == 0) abort(paste0("Can not find `ShinyDriver$new` in test file: ", test_path))
  # TODO-barret; split the code into parts and recurse
  if (length(init_line_pos) > 1) abort(paste0("Can not migrate file that contains multiple calls to `ShinyDriver$new`: ", test_path))
  test_text <- sub("ShinyDriver$new(", "ShinyDriver$public_methods$initialize(", test_text, fixed = TRUE)
  # cat(test_text, "\n")

  # Extract the app variable name
  matches <- regexpr('(^|\\n)\\s*(?<app>[^\\s]+)\\s*(=|<-)\\s*ShinyDriver\\$public_methods\\$initialize', test_text, perl = TRUE)
  app_txt_start <- attr(matches, "capture.start")[1, "app"]
  app_txt_len <- attr(matches, "capture.length")[1, "app"]
  app_var <- substr(test_text, app_txt_start, app_txt_start + app_txt_len - 1)
  info_env$app_var <- app_var

  # TODO-barret;
  # * Extract the ShinyDriver information
  # * Convert the text to possibly hit the `snapshotInit()` information
  # * Find the ShinyDriver$new() again
  # * Convert the ShinyDriver$new() -> AppDriver$new()



  ## Would be great to have control over how many lines are being sent.
  ## At this point, we know the variable being used and can use that information to very quickly update the content
  ## `app$setInputs(foo = app$getValues())` -> `app$set_inputs(new_foo = app$get_values())`

  # Extract the app variable name
  # For each known {shinytest} function call,
  #   Search for the function call: `APPVAR$FUNCTION(`
  #   Search for the closing parenthesis: `)`. See https://stackoverflow.com/questions/524548/regular-expression-to-detect-semi-colon-terminated-c-for-while-loops/524624#524624
  #     Will need to "tokenize" the line to find the closing parenthesis!
  #   For each match found (starting from the last match to the first match),
  #     Standardise the call to extract the arguments by replacing `APPVAR$FUNCTION` with `ShinyDriver$public_methods$FUNCTION`
  #     Replace the known text with the updated call: `APPVAR$NEW_FUNCTION(foo2=bar)`
  # Write the updated text

  # ## Will kinda keep comments; Will remove "within function" comments
  # While lines exist in the test lines queue...
  #   If comment / blank line,
  #     Write line; Pop line from queue
  #   else
  #    Feed in lines until it can successfully parse; Pop those lines from queue
  #    For each parsed value,
  #      Recurse over every parse value;
  #      For each parsed value,
  #        Match it against regexp info
  #        Standardise the call to extract the arguments
  #        Replace it with updated call
  #      Write the updated line

  migrate__algo_2(test_text, info_env)

  if (app_info_env$verbose) {
    rlang::inform(paste0("Writing: ", info_env$save_path))
  }
  write_utf8(test_text, info_env$save_path)

  c(as.list(info_env), list(test_exprs, test_lines))
}



# Copy over code to avoid being hosed down the road
shinytest___find_tests <- function (tests_dir, testnames = NULL){
  found_testnames <- list.files(tests_dir, pattern = "\\.[rR]$")
  found_testnames_no_ext <- sub("\\.[rR]$", "", found_testnames)
  if (!is.null(testnames)) {
      testnames_no_ext <- sub("\\.[rR]$", "", testnames)
      idx <- match(testnames_no_ext, found_testnames_no_ext)
      if (any(is.na(idx))) {
          abort(c("Test scripts do not exist:", testnames[is.na(idx)]))
      }
      found_testnames <- found_testnames[idx]
  }
  found_testnames
}


find_text_until_closing_paren <- function(txt) {
  arr <- strsplit(txt, "")[[1]]
  arr_len <- length(arr)

  cur_pos <- 1
  paren_count <- 0
  find_next_gen <- function(val, look_for_slash = FALSE) {
    function() {
      while (cur_pos <= arr_len) {
        char <- arr[cur_pos]
        if (char == val) {
          if (look_for_slash && cur_pos > 1) {
            if (arr[cur_pos - 1] == "\\") {
              # Lead by `\`; Continue like normal
            } else {
              # Not lead by a `\`, found the closer!
              return()
            }
          } else {
            # Found it!
            return()
          }
        }
        cur_pos <<- cur_pos + 1
      }
      abort(paste0("Could not find closing character: ", val))
    }
  }
  # find_closing_paren <- find_next_gen(")")
  find_closing_double_quote <- find_next_gen("\"", TRUE)
  find_closing_single_quote <- find_next_gen("'", TRUE)

  while (cur_pos <= arr_len) {
    char <- arr[cur_pos]
    switch(char,
      "'" = { find_closing_single_quote() },
      "\"" = { find_closing_double_quote() },
      "(" = {paren_count <- paren_count + 1},
      ")" = {
        paren_count <- paren_count - 1
        if (paren_count == 0) {
          return(paste0(arr[seq_len(cur_pos)], collapse = ""))
        }
      },
      default = {
        # do nothing
      }
    )

    cur_pos <- cur_pos + 1
  }

  abort(paste0("Could not find closing parenthesis in text: ", txt))
}



# ## Will kinda keep comments; Will remove "within function" comments
# While lines exist in the test lines queue...
#   If comment / blank line,
#     Write line; Pop line from queue
#   else
#    Feed in lines until it can successfully parse; Pop those lines from queue
#    For each parsed value,
#      Recurse over every parse value;
#      For each parsed value,
#        Match it against regexp info
#        Standardise the call to extract the arguments
#        Replace it with updated call
#      Write the updated line
migrate__algo_2 <- function(test_text, info_env) {
  test_lines <- strsplit(test_text, "\n")[[1]]
  test_lines_len <- length(test_lines)
  ret <- NULL
  cur_line <- 1
  while (cur_line <= test_lines_len) {
    line <- test_lines[cur_line]

    parsed_line <- try(parse(text = line), silent = TRUE)
    if (length(parsed_line) == 0) {
      # this is a comment or blank line, so just keep it as is
      ret[length(ret) + 1] <- line
      cur_line <- cur_line + 1
      next
    }

    start_line <- cur_line
    end_line <- cur_line
    exprs <- parsed_line
    if (inherits(parsed_line, "try-error")) {
      # This is a line that can not be parsed.
      # Keep trying to grab the next line until success
      next_line <- cur_line + 1
      if (next_line > test_lines_len) {
        abort(paste0("Can not parse final line of code starting at line: ", cur_line))
      }
      while (next_line <= test_lines_len) {
        parsed_lines <- try(
          parse(
            text = paste0(
              test_lines[cur_line: next_line],
              collapse = "\n"
            )
          ),
          silent = TRUE
        )
        if (!inherits(parsed_lines, "try-error")) {
          # We found the set of lines that can be parsed
          break
        }
        next_line <- next_line + 1
      }
      end_line <- next_line
      exprs <- parsed_lines
    }

    for (expr in exprs) {
      ret_expr <- migrate__shinytest_lang(expr, info_env)
      if (!is.null(ret_expr)) {
        ret <- append(
          ret,
          if (is.character(ret_expr))
            ret_expr
          else
            rlang::expr_text(ret_expr, width = 500L)
        )
      }
    }
    # Update cur_line pointer
    cur_line <- end_line + 1
  }

  paste0(ret, collapse = "\n")
}



migrate__shinytest_lang <- function(expr, info_env) {
  shinytest_lang_is_fn <- function(expr) {
    expr_fn <- expr[[1]]

    is.language(expr_fn) &&
      length(expr_fn) >= 3 &&
      expr_fn[[1]] == "$" &&
      expr_fn[[2]] == info_env$app_var
  }

  if (!is.language(expr)) {
    return(expr)
  }
  expr_list <- as.list(expr)

  # str(expr_list)
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
    i_val <- migrate__shinytest_lang(expr_list[[i]], info_env)
    # if (!is.list(i_val)) {
    #   str(i_val)
    #   return(paste0("Could not parse the (", i, ")th part of: ", rlang::expr_text(expr)))
    # }
    expr_list[[i]] <- i_val
  }

  # By being after the for-loop, it alters from the leaf to the trunk
  if (shinytest_lang_is_fn(expr_list)) {
    # match against known function names in expr[[3]]
    expr_list <- match_shinytest_expr(expr_list, info_env)
    if (!is.list(expr_list)) {
      return(expr_list)
    }
  }

  # Reconstruct language call
  rlang::call2(expr_list[[1]], !!!expr_list[-1])

}

# TODO-barret; test for no values_ values
# VALS\s*(<-|=)\s*APPVAR\s*\$\s*setInputs
# vals <- local({
#   app$set_inputs(x = 42)
#   app$get_values()
# })


match_shinytest_expr <- function(expr_list, info_env) {
  # message("Found expr!:")
  barret <<- expr_list
  expr_fn <- expr_list[[1]]
  app_fn_sym <- expr_fn[[3]]
  expr_args <- expr_list[-1]

  # message("expr_fn:"); str(expr_fn)
  # message("expr_args:"); str(expr_args)

  match_shinytest_args <- function(method, defaults = FALSE) {
    matched_call <- rlang::call_match(
      as.call(expr_list),
      shinytest::ShinyDriver$public_methods[[method]],
      defaults = defaults
    )
    rlang::call_args(matched_call)
  }
  shinytest2_expr_list <- function(method, args) {
    expr_fn[[3]] <- rlang::sym(method)
    rlang::list2(expr_fn, !!!args)
  }
  call2_fn <- function(fn_expr, ...) {
    call <- rlang::enexpr(fn_expr)
    rlang::call2(
      call,
      ...
    )
  }
  # Use `x` to determine if the value is a character
  paste0_call_maybe <- function(x, ...) {
    if (is.character(x)) {
      # `"#selector"`
      paste0(...)
    } else {
      # `paste0("#", selector)`
      call2_fn(paste0, ...)
    }
  }
  list_call <- function(...) {
    call2_fn(list, ...)
  }
  abort_if_not_character <- function(x, fn_name, arg_name) {
    if (!is.character(x)) {
      rlang::abort(paste0("`ShinyDriver$", fn_name, "(", arg_name, "=)` must be a character value, not a variable to be auto-converted by {shinytest2}"))
    }
  }
  iotype_arg <- function(matched_args, fn_name, types = c("auto", "input", "output")) {
    iotype <- matched_args$iotype %||% (types[1])
    abort_if_not_character(iotype, fn_name, "iotype")
    match.arg(iotype, types)
  }
  inform_js <- function(fn_name, arg_name) {
    rlang::inform(c(
      i = paste0("`ShinyDriver$", fn_name, "(", arg_name, "=)` would automatically return the last value. `AppDriver`'s `ChromoteSession` does not auto return the last value.",
      "x" = "Please add JavaScript `return` statements appropriately.")
    ))
  }

  switch(as.character(app_fn_sym),
    "setInputs" = {
      matched_args <- match_shinytest_args("setInputs")
      matched_args_names <- names(matched_args)
      if ("allowInputNoBinding_" %in% matched_args_names) {
        names(matched_args)[matched_args_names == "allowInputNoBinding_"] <- "allow_input_no_binding_"
      }
      # Yell about removed functionality
      if (!is_false(matched_args[["values_"]] %||% TRUE)) {
        rlang::abort("`ShinyDriver$setInputs(values_=)` is no longer supported. Use `AppDriver$get_values()` directly.")
      }
      matched_args$values_ <- NULL

      return(
        shinytest2_expr_list("set_inputs", matched_args)
      )
    },
    "click" = {
      matched_args <- match_shinytest_args("click")
      iotype <- iotype_arg(matched_args, "click")

      ## Convert `auto` to `selector`
      if (iotype == "auto") {
        iotype <- "selector"
        matched_args$name <- paste0_call_maybe(
          matched_args$name,
          "#", matched_args$name
        )
      }
      fn_args <- list()
      fn_args[[iotype]] <- matched_args$name
      return(
        shinytest2_expr_list("click", fn_args)
      )

    },
    "executeScript" = {
      inform_js("executeScript", "script")

      matched_args <- match_shinytest_args("executeScript")
      script <- matched_args$script
      script_args <- matched_args[!(names(matched_args) %in% "script")]
      fn_args <- list(script = script)
      if (length(script_args) > 0) {
        # Store arguments as a list of args as `language`
        fn_args[["arguments"]] <- list_call(!!!script_args)
      }
      # There was no timeout before.
      # Add at end to avoid surprises.
      fn_args[["timeout"]] <- 10000
      return(
        shinytest2_expr_list("execute_js", fn_args)
      )
    },
    "executeScriptAsync" = {
      abort("Please see the `shinytest-migration` vignette for an example on how to convert your `ShinyDriver$executeScriptAsync()` code to `AppDriver$execute_js()` using JavaScript Promises.")
    },
    "expectUpdate" = {
      # expr_fn[[3]] <- rlang::sym("get_log")

      matched_args <- match_shinytest_args("expectUpdate")
      iotype <- iotype_arg(matched_args, "expectUpdate")
      if (iotype == "output") {
        rlang::abort(c(
          "`ShinyDriver$expectUpdate(iotype = \"output\")` is not supported by `AppDriver`.",
          x = "Please set an `iotype = \"input\"`"
        ))
      }
      if (iotype == "auto") {
        rlang::inform(c(
          "!" = "`ShinyDriver$expectUpdate(iotype=)` is set to `\"auto\"`; Using `iotype = \"input\"`."
        ))
        iotype <- "input"
      }
      stopifnot(iotype == "input")

      # Required
      output_val <- matched_args$output
      input_vals <- matched_args[!(names(matched_args) %in% c("output", "iotype", "timeout"))]
      timeout_val <- matched_args$timeout %||% 3000
      app_val <- rlang::sym(info_env$app_var)

      # Use two different expressions depending on the type
      new_expr <- rlang::inject(rlang::expr(
        local({
          prior_output_value <- (!!app_val)$get_values(output = (!!output_val))
          (!!app_val)$set_inputs(!!!input_vals, timeout_ = !!timeout_val)
          new_output_value <- (!!app_val)$get_values(output = (!!output_val))
          testthat::expect_failure(
            testthat::expect_equal(
              new_output_value,
              prior_output_value
            )
          )
        })
      ))
      # Return full _complicated_ subroutine
      return(as.list(new_expr))
    },
    "findElement" = ,
    "findElements" = ,
    "findWidget" = {
      abort(paste0("Please see the `shinytest-migration` vignette for an example on how to convert your `ShinyDriver$", as.character(app_fn_sym), "()` to be {shinytest2} friendly."))
    },

    "getAllValues" = {
      matched_args <- match_shinytest_args("getAllValues")
      matched_args$hash_images = FALSE
      return(
        shinytest2_expr_list("get_values", matched_args)
      )

    },

    "isRmd" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::inject(rlang::expr(
        local({
          path <- (!!app_val)$get_path()
          fs::path_ext(path) == ".Rmd"
        })
      ))
      return(as.list(new_expr))
    },
    "getAppDir" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::inject(rlang::expr(
        local({
          path <- (!!app_val)$get_path()
          if (fs::path_ext(path) == ".Rmd") {
            fs::path_dir(path)
          } else {
            path
          }
        })
      ))
      return(as.list(new_expr))
    },
    "getAppFilename" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::inject(rlang::expr(
        local({
          path <- (!!app_val)$get_path()
          if (fs::path_ext(path) == ".Rmd") {
            fs::path_file(path)
          } else {
            NULL
          }
        })
      ))
      return(as.list(new_expr))
    },

    "enableDebugLogMessages" = {
      rlang::inform(c(
        i = "`ShinyDriver$enableDebugLogMessages()` is not implemented in `AppDriver`. All debug messages are always recorded in {shinytest2}.",
        "!" = "Removing call to `ShinyDriver$enableDebugLogMessages()`"
      ))
      return(NULL)
    },
    "getDebugLog" = ,
    "getEventLog" = {
      rlang::inform(c(
        i = paste0("`ShinyDriver$", as.character(app_fn_sym), "()` is not implemented in `AppDriver`."),
        "!" = "A single `AppDriver$get_log()` method should be used."
      ))

      return(shinytest2_expr_list("get_log", list()))
    },

    "getSnapshotDir" = ,
    "getRelativePathToApp" = ,
    "getTestsDir" = {
      rlang::abort(c(
        i = paste0("`ShinyDriver$", as.character(app_fn_sym), "()` is not implemented in `AppDriver`. {shinytest2} is integrated with {testthat} and `AppDriver` does not support non-standard test file locations."),
        "x" = "Please remove or replace this function call`"
      ))
    },

    "getSource" = {
      return(shinytest2_expr_list(
        "get_html", list("html", outer_html = TRUE)
      ))
    },
    "getTitle" = {
      return(shinytest2_expr_list(
        "execute_js", list("return window.document.title;")
      ))
    },
    "getUrl" = {
      return(shinytest2_expr_list(
        "get_url", list()
      ))
    },

    "getValue" = {
      rlang::inform(c(
        i = "`ShinyDriver$getValue()` is not implemented in `AppDriver`. It relied on invasive Shiny logic.",
        "!" = "Replacing this with a generic call to `AppDriver$get_values()`"
      ))

      matched_args <- match_shinytest_args("getValue")
      iotype <- iotype_arg(matched_args, "getValue")
      if (iotype == "auto") {
        rlang::inform(c(
          "!" = "`ShinyDriver$getValue(iotype=)` is set to `\"auto\"`; Using `output` as a guess."
        ))
        iotype <- "output"
      }
      fn_args <- list()
      fn_args[[iotype]] <- matched_args$name
      app_val <- rlang::sym(info_env$app_var)
      return(rlang::inject(rlang::expr(
        (!!app_val)$get_values(!!!fn_args)[[!!iotype]][[(!!matched_args$name)]]
      )))
    },
    "setValue" = {

      matched_args <- match_shinytest_args("setValue")
      iotype <- iotype_arg(matched_args, "setValue")
      if (iotype == "output") {
        rlang::abort(
          "`ShinyDriver$setValue(iotype=)` is set to `\"output\"`; {shinytest2} does not support setting output values directly."
        )
      }
      rlang::inform(c(
        i = "`ShinyDriver$setValue()` is not implemented in `AppDriver`. It relied on invasive Shiny logic.",
        "!" = "Replacing this with a call to `AppDriver$set_inputs()`"
      ))
      if (iotype == "auto") {
        rlang::inform(c(
          "!" = "`ShinyDriver$setValue(iotype=)` is set to `\"auto\"`; Using `input` as a guess."
        ))
        iotype <- "input"
      }

      fn_args <- list()
      fn_args[[matched_args$name]] <- matched_args$value
      return(
        shinytest2_expr_list("set_inputs", fn_args)
      )
    },

    "getWindowSize" = {
      return(
        shinytest2_expr_list("get_window_size", list())
      )
    },
    "setWindowSize" = {
      matched_args <- match_shinytest_args("setWindowSize")
      return(
        shinytest2_expr_list("set_window_size", matched_args)
      )
    },

    "goBack" = {
      return(
        shinytest2_expr_list("execute_js", list("window.history.back();"))
      )
    },
    "refresh" = {
      return(
        shinytest2_expr_list("execute_js", list("window.location.reload();"))
      )
    },
    "clone" = {
      rlang::abort("`AppDriver$clone()` is not supported.")
      return(
        shinytest2_expr_list("execute_js", list("window.location.reload();"))
      )
    },

    "listWidgets" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::inject(rlang::expr(
        lapply((!!app_val)$get_values(), names)
      ))
      return(as.list(new_expr))
    },

    "logEvent" = {
      matched_args <- match_shinytest_args("logEvent")
      if (length(matched_args) > 1) {
        rlang::abort(c(
          "`AppDriver$log_message()` does not support multiple arguments.",
          x = "Please store your message into a single argument."
        ))
      }
      if (length(matched_args) == 0) {
        rlang::abort(c(
          "`AppDriver$log_message()` requires a message argument."
        ))
      }

      # Like `list(msg)` to remove the prior name
      # and not provide a name to the shinytest2 function
      fn_args <- list(matched_args[[1]])

      return(
        shinytest2_expr_list("log_message", fn_args)
      )
    },

    "sendKeys" = {
      rlang::abort("{shinytest2} does not support `$sendKeys()`")
    },

    "snapshotInit" = {
      rlang::inform(c(
        i = "`ShinyDriver$snapshotInit()` is not implemented in `AppDriver`. The `path` and `screenshot` information should be moved to `AppDriver$new()`"
      ))

      matched_args <- match_shinytest_args("snapshotInit", defaults = TRUE)
      name <- matched_args$path
      abort_if_not_character(name, "snapshotInit", "path")
      info_env$name <- name
      info_env$screenshot <- matched_args$screenshot

      # No replacement code
      return(NULL)
    },

    "snapshot" = {
      matched_args <- match_shinytest_args("snapshot")
      values_args <- list()
      pic_args <- list()

      items <- matched_args$items
      if (!is.null(items)) {
        items_list <- as.list(items)
        if (
          !(
            is.language(items) &&
            !is.symbol(items) &&
            items_list[[1]] == "list" &&
            length(items_list) > 1 &&
            all(
              rlang::names2(items_list[-1]) %in% c("input", "output", "export")
            )
          )
        ) {
          rlang::abort(c(
            "`ShinyDriver$snapshot(items=)` can only be auto converted if `items` is missing, `NULL`, or a list of `input`, `output`, and/or `export`. Variables may not be used!"
          ))
        }
        values_args <- items_list[-1]
      }

      take_screenshot <- matched_args$screenshot %||% TRUE
      if (isTRUE(take_screenshot)) {
        if (!is.null(matched_args$filename)) {
          pic_args$name <- matched_args$filename
        }
      }

      app_var <- rlang::sym(info_env$app_var)
      new_expr <-
        if (take_screenshot) {
          # take a screenshot and values
          rlang::inject(rlang::expr(
            local({
              (!!app_var)$expect_values(!!!values_args)
              (!!app_var)$expect_screenshot(!!!pic_args)
            })
          ))
        } else {
          # No screenshot, only values
          rlang::inject(rlang::expr(
            (!!app_var)$expect_values(!!!values_args)
          ))
        }

      return(new_expr)
    },

    "snapshotDownload" = {
      matched_args <- match_shinytest_args("snapshotDownload")
      fn_args <- list(matched_args$id)
      if (!is.null(matched_args$filename)) {
        fn_args$name <- matched_args$filename
      }
      return(
        shinytest2_expr_list("expect_download", fn_args)
      )
    },

    "stop" = {
      return(
        shinytest2_expr_list("stop", list())
      )
    },

    "takeScreenshot" = {
      # screenshot
      matched_args <- match_shinytest_args("takeScreenshot")
      fn_args <- list(
        matched_args$file
      )
      if (!is.null(matched_args$id)) {
        fn_args$selector <- paste0_call_maybe(
          matched_args$id,
          "#", matched_args$id
        )
      }
      if (
        "parent" %in% names(matched_args) &&
        !is_false(matched_args$parent)
      ) {
        rlang::abort(c(
          "`ShinyDriver$takeScreenshot(parent=)` is not supported in {shinytest2}. Currently, CSS `parent` selector is not officially supported.",
          x = "Please provide a better `AppDriver$screenshot(selector=)` value."
        ))
      }
      return(
        shinytest2_expr_list("screenshot", fn_args)
      )
    },

    "uploadFile" = {
      # upload_file
      matched_args <- match_shinytest_args("uploadFile")

      # Yell about removed functionality
      if (!is_false(matched_args[["values_"]] %||% TRUE)) {
        rlang::abort("`ShinyDriver$uploadFile(values_=)` is no longer supported. Use `AppDriver$get_values()` directly.")
      }
      matched_args[["values_"]] <- NULL

      return(
        shinytest2_expr_list("upload_file", matched_args)
      )
    },

    "waitFor" = {
      # wait_for_js
      inform_js("waitFor", "expr")
      matched_args <- match_shinytest_args("waitForShiny", defaults = TRUE)

      fn_args <- list(
        matched_args$expr,
        duration = 0,
        timeout = matched_args$timeout
      )

      return(
        shinytest2_expr_list("wait_for_js", fn_args)
      )
    },

    "waitForShiny" = {
      return(
        shinytest2_expr_list(
          "wait_for_idle", list(duration = 0)
        )
      )
    },
    "waitForValue" = {
      matched_args <- match_shinytest_args("waitForValue")
      iotype <- iotype_arg(matched_args, "waitForValue", types = c("input", "output", "export"))
      fn_args <- list()
      fn_args[[iotype]] <- matched_args$name
      fn_args$ignore <- matched_args$ignore
      fn_args$timeout <- matched_args$timeout
      fn_args$interval <- matched_args$checkInterval
      return(
        shinytest2_expr_list("wait_for_value", fn_args)
      )
    },



    # "getEventLog" = {
    #   expr_fn[[3]] <- rlang::sym("get_log")
    # },
    abort(paste0("Unknown method: ", as.character(app_fn_sym)))
  )

  # Reconstruct call list
  rlang::list2(expr_fn, !!!expr_args)
}
