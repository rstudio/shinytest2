# TOOD-barret-implement: new$get_value(input = "myinput") # validate that `key` length is 1
# TODO-barret; Make test app using allowInputNoBinding = TRUE



#' Migrate shinytest tests
#'
#' This function will migrate standard shinytest test files to the new \pkg{shinytest2} + \pkg{testthat} ed 3 snapshot format.
#'
#' \pkg{shinytest} file contents will be traversed and converted to the new \pkg{shinytest2} format. If the \pkg{shinytest} code can not be directly seen in the code, then it will not be converted.
#'
#' @param path Path to the test directory or Shiny Rmd file
#' @param ... Must be empty. Allows for parameter expansion.
#' @param include_expected_screenshot If `TRUE`, `ShinyDriver$snapshot()` will turn into both `AppDriver$expect_values()` and `AppDriver$expect_screenshot()`. If `FALSE`, `ShinyDriver$snapshot()` will only turn into `AppDriver$expect_values()`. If missing, `include_expected_screenshot` will behave as `FALSE` if `shinytest::testApp(compareImages = FALSE)` or `ShinyDriver$snapshotInit(screenshot = FALSE)` is called.
#' @return
migrate <- function(path, ..., include_expect_screenshot = missing_arg(), quiet = FALSE) {
  ellipsis::check_dots_empty()

  path_info <- app_path(path, "path")

  if (TRUE) {
    # TODO-barret; Remove this code
    new_path <- fs::path(fs::path_dir(path_info$dir), paste0(fs::path_file(path_info$dir), "-tmp"))
    rlang::inform(paste0("temp copying over path: ", new_path))
    if (fs::dir_exists(new_path)) fs::dir_delete(new_path)
    fs::dir_copy(path_info$dir, new_path)
    path_info <- app_path(new_path, "path")
  }

  # Use an environment to avoid having to return many function levels to merge info and then send back many function levels
  app_info_env <- as.environment(path_info)
  app_info_env$verbose <- !isTRUE(quiet)
  app_info_env$include_expect_screenshot <- include_expect_screenshot

  rlang::inform(c(i = paste0("Temp working directory: ", path_info$dir)))
  withr::with_dir(path_info$dir, {
    migrate__extract_runner_info(app_info_env)
    migrate__write_shinytest2_runner(app_info_env)

    invisible(
      migrate__parse_test_files(app_info_env)
    )
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

  # TODO-future; Make this recursive and quit once we find the first `shinytest::testApp`
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
    test_call_args <- rlang::call_args(rlang::call_match(expr, shinytest::testApp))
    if (fs::path_rel(test_call_args$appDir) != "..") {
      abort(paste0(
        "shinytest::testApp() must be called on the parent directory.\n",
        "{shinytest2} does not know how to automatically migrate this app."
      ))
    }
    # Store knowledge
    app_info_env$test_names <-
      shinytest___find_tests("tests/shinytest", test_call_args$testnames)
    # Eventually always set the variant to the suffix to allow for $expect_screenshot() to work
    app_info_env$suffix <- test_call_args$suffix
    app_info_env$compare_images <- test_call_args$compareImages %||% TRUE

    # Nothing else to find. Quit early
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
    rlang::inform(c("i" = paste0("`suffix` found: ", rlang::maybe_missing(app_info_env$suffix, ""))))
    rlang::inform(c("i" = paste0("`testnames` found: ", paste0(app_info_env$test_names, collapse = ", "))))
  }

  lapply(app_info_env$test_names, function(test_name) {
    test_path <- file.path("tests/shinytest", test_name)
    migrate__parse_test_file(test_path, app_info_env)
  })

}

migrate__reset_info_env <- function(app_info_env) {
  rm(
    c(
      "test_path",
      "save_path",
      "app_var",
      "name",
      "screenshot_snapshot_init",
      "match_found"
      # Not `take_screenshot`; We want this value to persist
    ),
    envir = app_info_env
  )
  app_info_env
}

migrate__parse_test_file <- function(test_path, app_info_env) {
  if (app_info_env$verbose) {
    rlang::inform(c("i" = paste0("Migrating test", test_path)))
  }

  # Reset the environment for the next file by removing flags / prior file knowledge
  info_env <- migrate__reset_info_env(app_info_env)
  info_env$test_path <- test_path
  info_env$save_path <- fs::path("tests", "testthat", paste0("test-", fs::path_file(test_path)))

  test_text <- read_utf8(test_path)
  migrated_text <- migrate__parse_test_text(test_text, test_path, info_env)

  if (length(migrated_text) == 0) {
    # TODO-barret; test this; does it work?
    abort("Needs testing")
    if (info_env$verbose) {
      rlang::inform(paste0("No test content found in `{test_path}`"))
      rlang::inform(paste0("Creating: ", info_env$save_path))
    }
    file.copy(test_path, info_env$save_path)
    return()
  }

  if (app_info_env$verbose) {
    rlang::inform(c(i = paste0("Writing: ", info_env$save_path)))
  }
  write_utf8(migrated_text, info_env$save_path)
}

migrate__parse_test_text <- function(test_text, test_path, info_env) {
  test_lines <- strsplit(test_text, "\n")[[1]]

  if (length(test_lines) == 0) {
    return(character(0))
  }

  # Extract the app variable name
  # TODO-barret; use regex? Or use a parser?
  # TODO-barret-answer; Use regex to help with line support
  matches <- gregexpr('(^|\\n)\\s*(?<app>[^\\s]+)\\s*(=|<-)\\s*ShinyDriver\\$new', test_text, perl = TRUE)[[1]]
  if (length(matches) == 0) abort(paste0("Can not find `ShinyDriver$new` in test file: ", test_path))
  # TODO-future; split the code into parts and recurse
  if (length(matches) > 1) abort(paste0("Can not migrate file that contains multiple calls to `ShinyDriver$new`: ", test_path))
  app_txt_start <- attr(matches, "capture.start")[1, "app"]
  app_txt_len <- attr(matches, "capture.length")[1, "app"]
  app_var <- substr(test_text, app_txt_start, app_txt_start + app_txt_len - 1)
  info_env$app_var <- app_var

  ## Depending on the methods called (ex: $snapshotInit()),
  ## AppDriver$new will have different arg values
  # * Extract the external ShinyDriver information
  # * Convert the text to possibly hit the `snapshotInit()` information
  # * Find the ShinyDriver$new() again
  # * Extract the ShinyDriver args
  # * Convert the ShinyDriver$new() -> AppDriver$new()

  # At this point, we know the variable name being used and
  # can use that information to very quickly update the content
  # Ex: `app$setInputs(foo = app$getValues())` -> `app$set_inputs(new_foo = app$get_values())`
  migrated_text <- migrate__algo_2(test_text, info_env)
  migrated_lines <- strsplit(migrated_text, "\n")[[1]]
  init_line <- which(grepl("^[^#]*ShinyDriver\\$new\\s*\\(", migrated_lines, fixed = FALSE))
  parsed_info <- parse_next_expr(migrated_lines[seq(from = init_line, to = length(migrated_lines))])

  parsed_expr_text <- get_each_expr_text(
    parsed_info$exprs,
    migrate__driver_init, info_env
  )
  # Remove the previous init code
  migrated_lines <- migrated_lines[-1 * seq(from = init_line, by = 1, length.out = parsed_info$n)]
  # Add the new init code
  migrated_lines <- append(migrated_lines, parsed_expr_text, after = init_line - 1)

  # Return a large string
  paste0(migrated_lines, collapse = "\n")
}

migrate__driver_init <- function(expr, info_env) {
  expr_list <- as.list(expr)
  complete <- force
  if (as.character(expr_list[[1]]) %in% c("<-", "=")) {
    expr_name <- expr_list[[2]]
    complete <- function(ex) {
      rlang::call2("<-", expr_name, ex)
    }
    expr_list <- as.list(expr_list[[3]])
  }

  if (st2_expr_text(expr_list[[1]]) != "ShinyDriver$new") {
    return(expr)
  }
  expr_args <-
    rlang::call_args(rlang::call_match(
      as.call(expr_list),
      shinytest::ShinyDriver$public_methods$initialize,
      defaults = FALSE
    ))
  expr_args_names <- names(expr_args)

  init_args <- list()
  if ("path" %in% expr_args_names) {
    if (fs::path_rel(expr_args$path) != "../..")
    init_args[[1]] <- expr_args$path
  }
  init_args$variant <- rlang::maybe_missing(info_env$suffix, NULL)
  if (!is.null(info_env$name)) {
    init_args$name <- info_env$name
  }
  # if (!is.null(info_env$screenshot)) {
  #   if (is_false(info_env$screenshot)) {
  #     init_args$screenshot_args <- FALSE
  #   }
  # }
  for (name_info in list(
    list(from = "loadTimeout", to = "load_timeout"),
    list(from = "checkNames", to = "check_names"),
    list(from = "seed", to = "seed"),
    list(from = "cleanLogs", to = "clean_logs"),
    list(from = "shinyOptions", to = "shiny_args"),
    list(from = "renderArgs", to = "render_args"),
    list(from = "options", to = "options")
  )) {
    if (name_info$from %in% expr_args_names) {
      init_args[[name_info$to]] <- expr_args[[name_info$from]]
    }
  }
  if ("debug" %in% expr_args_names) {
    rlang::inform("`ShinyDriver$new(debug=)` is not supported by `AppDriver`. All debugging messages are always recorded.")
  }
  if ("phantomTimeout" %in% expr_args_names) {
    rlang::inform("`ShinyDriver$new(phantomTimeout=)` is not supported by `AppDriver`. `{chromote}` does not have a timeout parameter.")
  }

  ret_expr <- rlang::call2(
    rlang::expr(AppDriver$new),
    !!!init_args
  )
  complete(ret_expr)
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


# Starting with the first line (first element of `texts`),
# keep adding lines to parse until a successful parse is found.
# Return
# * Number of lines parsed
# * The raw text
# * The parsed expr
parse_next_expr <- function(texts) {
  texts_len <- length(texts)
  start <- 1
  end <- start
  lines <- NULL
  exprs <- NULL

  while (TRUE) {
    lines <- texts[start:end]
    exprs <- try(
      parse(
        # Works with vectors of text as one big expression
        text = lines
      ),
      silent = TRUE
    )
    if (!inherits(exprs, "try-error")) {
      # We found the set of lines that can be parsed. Yay!
      break
    }
    end <- end + 1
    if (end > texts_len) {
      rlang::abort(paste0("Can not parse texts:\n", paste0(texts, collapse = "\n")))
    }
  }

  return(list(
    n = end,
    lines = lines,
    exprs = exprs
  ))
}



# ## Will remove "within function" comments; Will preserve comments outside of R code
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
  # cur_line <- 1
  while (length(test_lines) > 0) {
    parsed_info <- parse_next_expr(test_lines)
    # `parse()` returns a list of expression values
    exprs <- parsed_info$exprs
    lines <- parsed_info$lines
    n <- parsed_info$n

    if (length(exprs) == 0) {
      # This is a comment or blank line, so just keep it as is
      ret[length(ret) + 1] <- lines
    } else {
      info_env$match_found <- FALSE
      expr_texts <- get_each_expr_text(
        exprs,
        migrate__shinytest_lang,
        info_env,
        # Use a flag to declare if a match is found in the parsed line
        is_top_level = TRUE
      )
      # If no match is found, return the original text
      ret <- append(
        ret,
        if (is_false(info_env$match_found)) lines
        else expr_texts
      )
    }

    # Remove lines
    test_lines <- test_lines[-1 * seq_len(n)]
  }

  paste0(ret, collapse = "\n")
}



migrate__shinytest_lang <- function(expr, info_env, is_top_level = FALSE) {
  shinytest_lang_is_fn <- function(expr_list) {
    expr_fn <- expr_list[[1]]

    is.language(expr_fn) &&
      length(expr_fn) >= 3 &&
      expr_fn[[1]] == "$" &&
      expr_fn[[2]] == info_env$app_var
  }

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
  # # Some methods return a list of values
  # new_expr_list <- list()
  # for (expr_list_item in expr_list) {
  #   new_expr_list <- append(
  #     new_expr_list,
  #     migrate__shinytest_lang(expr_list[[i]], info_env, is_top_level = FALSE)
  #   )
  # }
  for (i in seq_len(length(expr_list))) {
    expr_list[[i]] <-
      migrate__shinytest_lang(expr_list[[i]], info_env, is_top_level = FALSE)
  }

  if (!shinytest_lang_is_fn(expr_list)) {
    return(
      # Reconstruct language call
      rlang::call2(expr_list[[1]], !!!expr_list[-1])
    )
  }

  # By being after the for-loop, it alters from the leaf to the trunk
  # Mark that a match was found
  info_env$match_found <- TRUE
  # Match against known function names in expr_list[[3]]
  matched_expr <- match_shinytest_expr(expr_list, is_top_level, info_env)
  matched_expr
}


match_shinytest_expr <- function(expr_list, is_top_level, info_env) {
  # message("Found expr!:")
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
  shinytest2_expr <- function(method, args) {
    expr_fn[[3]] <- rlang::sym(method)
    rlang::call2(expr_fn, !!!args)
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
      if (!is_top_level) {
        # Yell about removed functionality
        if (!is_false(matched_args[["values_"]] %||% TRUE)) {
          rlang::abort("`ShinyDriver$setInputs(values_=)` is no longer supported. Use `AppDriver$get_values()` directly. (This message was thrown because `ShinyDriver$setInputs()`'s result is possibly used.)")
        }
      }
      matched_args$values_ <- NULL

      shinytest2_expr("set_inputs", matched_args)
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
      shinytest2_expr("click", fn_args)
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
      shinytest2_expr("execute_js", fn_args)
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
      new_expr <- rlang::expr(
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
      )
      # Return full _complicated_ subroutine
      new_expr
    },
    "findElement" = ,
    "findElements" = ,
    "findWidget" = {
      abort(paste0("Please see the `shinytest-migration` vignette for an example on how to convert your `ShinyDriver$", as.character(app_fn_sym), "()` to be {shinytest2} friendly."))
    },

    "getAllValues" = {
      # Make sure the missing `TRUE` values are added
      matched_args <- match_shinytest_args("getAllValues", defaults = TRUE)
      # # Do not include this by default
      # matched_args$hash_images = FALSE
      shinytest2_expr("get_values", match_shinytest_args("getAllValues"))
    },

    "isRmd" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::expr(
        local({
          path <- (!!app_val)$get_path()
          fs::path_ext(path) == ".Rmd"
        })
      )
      new_expr
    },
    "getAppDir" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::expr(
        local({
          path <- (!!app_val)$get_path()
          if (fs::path_ext(path) == ".Rmd") {
            fs::path_dir(path)
          } else {
            path
          }
        })
      )
      new_expr
    },
    "getAppFilename" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::expr(
        local({
          path <- (!!app_val)$get_path()
          if (fs::path_ext(path) == ".Rmd") {
            fs::path_file(path)
          } else {
            NULL
          }
        })
      )
      new_expr
    },

    "enableDebugLogMessages" = {
      rlang::inform(c(
        i = "`ShinyDriver$enableDebugLogMessages()` is not implemented in `AppDriver`. All debug messages are always recorded in {shinytest2}.",
        "!" = "Removing call to `ShinyDriver$enableDebugLogMessages()`"
      ))
      NULL
    },
    "getDebugLog" = ,
    "getEventLog" = {
      rlang::inform(c(
        i = paste0("`ShinyDriver$", as.character(app_fn_sym), "()` is not implemented in `AppDriver`."),
        "!" = "A single `AppDriver$get_log()` method should be used."
      ))

      shinytest2_expr("get_log", list())
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
      shinytest2_expr("get_html", list("html", outer_html = TRUE))
    },
    "getTitle" = {
      shinytest2_expr("execute_js", list("return window.document.title;"))
    },
    "getUrl" = {
      shinytest2_expr("get_url", list())
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
      rlang::expr(
        (!!app_val)$get_values(!!!fn_args)[[!!iotype]][[(!!matched_args$name)]]
      )
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
      shinytest2_expr("set_inputs", fn_args)
    },

    "getWindowSize" = {
      shinytest2_expr("get_window_size", list())
    },
    "setWindowSize" = {
      matched_args <- match_shinytest_args("setWindowSize")
      shinytest2_expr("set_window_size", matched_args)
    },

    "goBack" = {
      shinytest2_expr("execute_js", list("window.history.back();"))
    },
    "refresh" = {
      shinytest2_expr("execute_js", list("window.location.reload();"))
    },
    "clone" = {
      rlang::abort("`AppDriver$clone()` is not supported.")
    },

    "listWidgets" = {
      app_val <- rlang::sym(info_env$app_var)
      new_expr <- rlang::expr(
        lapply((!!app_val)$get_values(), names)
      )
      new_expr
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

      shinytest2_expr("log_message", fn_args)
    },

    "sendKeys" = {
      rlang::abort("{shinytest2} does not support `$sendKeys()`")
    },

    "snapshotInit" = {
      rlang::inform(c(
        i = "`ShinyDriver$snapshotInit()` is not implemented in `AppDriver`.",
        "*" = "`ShinyDriver$snapshotInit(path=)` will become `AppDriver$initialize(name=)`",
        "*" = "`ShinyDriver$snapshotInit(screenshot=)` will help determine if `AppDriver$expect_screenshot()` will be provided alongside `AppDriver$expect_values()` when replacing `ShinyDriver$snapshot()`"
      ))

      matched_args <- match_shinytest_args("snapshotInit", defaults = TRUE)
      name <- matched_args$path
      abort_if_not_character(name, "snapshotInit", "path")
      info_env$name <- name
      info_env$screenshot_snapshot_init <- matched_args$screenshot

      # No replacement code
      NULL
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

      # Preference value:
      # 0. If exists, User preference value
      # 0. If exists, Prior user prompt value
      # 1. FALSE if compareImages == FALSE
      # 2. FALSE if args$screenshot == FALSE
      # 3. FALSE if snapshotInitArgs$screenshot == FALSE
      # 4. User prompt value
      take_screenshot <- local({
        if (!is.null(
          rlang::maybe_missing(info_env$include_expect_screenshot, NULL)
        )) {
          return(info_env$include_expect_screenshot)
        }

        if (!is.null(info_env$take_screenshot)) {
          return(info_env$take_screenshot)
        }

        if (
          is_false(info_env$compare_images) ||
          is_false(matched_args$screenshot) ||
          is_false(info_env$screenshot_snapshot_init)
        ) {
          return(FALSE)
        }

        if (
          isTRUE(matched_args$screenshot) ||
          isTRUE(info_env$screenshot_snapshot_init)
          ## Do not use as this is default behavior we want to avoid
          # isTRUE(info_env$compare_images)
        ) {
          return(TRUE)
        }

        if (!rlang::is_interactive()) {
          rlang::abort(c(
            "`ShinyDriver$snapshot()` needs instructions on whether to take a screenshot.",
            x = "Please set `include_expect_screenshot` to `TRUE` or `FALSE`."
          ))
        }
        ans <- utils::menu(
          graphics = FALSE,
          title = "TITLE!!!",
          choices = c(
            "No `AppDriver$expect_screenshot()` (recommended)",
            "Include `AppDriver$expect_screenshot()` (My tests will be brittle)"
          )
        )
        if (ans == 0) {
          rlang::inform(c(i = "Menu cancelled. No `AppDriver$expect_screenshot()` will be provided."))
          ans <- 1
        }
        info_env$take_screenshot <- ans != 2
        info_env$take_screenshot
      })
      # take_screenshot <-
      #   isTRUE(info_env$compare_images) &&
      #   isTRUE(info_env$screenshot %||% TRUE) &&
      #   (matched_args$screenshot %||% TRUE)
      if (isTRUE(take_screenshot)) {
        if (!is.null(matched_args$filename)) {
          pic_args$name <- matched_args$filename
        }
      }

      app_var <- rlang::sym(info_env$app_var)
      new_expr <-
        if (take_screenshot) {
          # take a screenshot and values
          rlang::exprs(
            (!!app_var)$expect_values(!!!values_args),
            (!!app_var)$expect_screenshot(!!!pic_args)
          )
        } else {
          # No screenshot, only values
          rlang::expr(
            (!!app_var)$expect_values(!!!values_args)
          )
        }
      new_expr
    },

    "snapshotDownload" = {
      matched_args <- match_shinytest_args("snapshotDownload")
      fn_args <- list(matched_args$id)
      if (!is.null(matched_args$filename)) {
        fn_args$name <- matched_args$filename
      }
      shinytest2_expr("expect_download", fn_args)
    },

    "stop" = {
      shinytest2_expr("stop", list())
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
      shinytest2_expr("screenshot", fn_args)
    },

    "uploadFile" = {
      # upload_file
      matched_args <- match_shinytest_args("uploadFile")

      if (!is_top_level) {
        # Yell about removed functionality
        if (!is_false(matched_args[["values_"]] %||% TRUE)) {
          rlang::abort("`ShinyDriver$uploadFile(values_=)` is no longer supported. Use `AppDriver$get_values()` directly. (This message was thrown because `ShinyDriver$uploadFile()`'s result is possibly used.)")
        }
      }
      matched_args[["values_"]] <- NULL

      shinytest2_expr("upload_file", matched_args)
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

      shinytest2_expr("wait_for_js", fn_args)
    },

    "waitForShiny" = {
      shinytest2_expr("wait_for_idle", list(duration = 0))
    },
    "waitForValue" = {
      matched_args <- match_shinytest_args("waitForValue")
      iotype <- iotype_arg(matched_args, "waitForValue", types = c("input", "output", "export"))
      fn_args <- list()
      fn_args[[iotype]] <- matched_args$name
      fn_args$ignore <- matched_args$ignore
      fn_args$timeout <- matched_args$timeout
      fn_args$interval <- matched_args$checkInterval
      shinytest2_expr("wait_for_value", fn_args)
    },


    # "getEventLog" = {
    #   expr_fn[[3]] <- rlang::sym("get_log")
    # },
    abort(paste0("Unknown method: ", as.character(app_fn_sym)))
  )
}


st2_expr_text <- function(expr) {
  if (is.null(expr) || is.character(expr)) return(expr)
  if (is.list(expr)) return(lapply(expr, st2_expr_text))
  gsub(
    "\\s*\n    ",
    "\n  ",
    rlang::expr_text(expr, width = 60L)
  )
}
get_each_expr_text <- function(exprs, fn, ...) {
  unlist(lapply(exprs, function(expr) {
    st2_expr_text(fn(expr, ...))
  }))
}
