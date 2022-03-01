# TODO-barret; Add more tests around `migrate_from_shinytest()`
# TODO-barret; Make test app using allowInputNoBinding = TRUE



#' Migrate shinytest tests
#'
#' This function will migrate standard shinytest test files to the new \pkg{shinytest2} + \pkg{testthat} ed 3 snapshot format.
#'
#' \pkg{shinytest} file contents will be traversed and converted to the new \pkg{shinytest2} format. If the \pkg{shinytest} code can not be directly seen in the code, then it will not be converted.
#'
#' @param path Path to the test directory or Shiny Rmd file
#' @param ... Must be empty. Allows for parameter expansion.
#' @param clean If TRUE, then the shinytest test directory and runner will be deleted after the migration to use \pkg{shinytest2}.
#' @param include_expect_screenshot If `TRUE`, `ShinyDriver$snapshot()` will turn into both `AppDriver$expect_values()` and `AppDriver$expect_screenshot()`. If `FALSE`, `ShinyDriver$snapshot()` will only turn into `AppDriver$expect_values()`. If missing, `include_expect_screenshot` will behave as `FALSE` if `shinytest::testApp(compareImages = FALSE)` or `ShinyDriver$snapshotInit(screenshot = FALSE)` is called.
#' @param quiet Logical that determines if migration information and steps should be printed to the console.
#' @return Invisible `TRUE`
#' @export
migrate_from_shinytest <- function(
  path,
  ...,
  clean = TRUE,
  include_expect_screenshot = missing_arg(),
  quiet = FALSE
) {
  ellipsis::check_dots_empty()
  rlang::check_installed("shinytest", version = "1.5.1")

  path_info <- app_path(path, "path")

  # Use an environment to avoid having to return many function levels to merge info and then send back many function levels
  app_info_env <- as.environment(path_info)
  app_info_env$verbose <- is_false(quiet)
  app_info_env$include_expect_screenshot <- include_expect_screenshot

  if (app_info_env$verbose) rlang::inform(c(i = paste0("Temp working directory: ", path_info$dir)))
  withr::with_dir(path_info$dir, {
    m__extract_runner_info(app_info_env)
    m__write_shinytest2_runner(app_info_env)
    m__parse_test_files(app_info_env)
    if (isTRUE(clean)) m__remove_shinytest_files(app_info_env)
  })

  invisible(TRUE)
}

m__remove_shinytest_files <- function(app_info_env) {

  files_to_remove <- c(
    app_info_env$shinytest_runner_file,
    if (fs::dir_exists("tests/shinytest")) dir("tests/shinytest", full.names = TRUE, recursive = TRUE)
  )
  if (app_info_env$verbose) {
    rlang::inform(c(
      i = "Removing shinytest files: ",
      stats::setNames(files_to_remove, rep("*", length(files_to_remove)))
    ))
  }

  fs::file_delete(app_info_env$shinytest_runner_file)
  if (fs::dir_exists("tests/shinytest")) {
    fs::dir_delete("tests/shinytest")
  }
}

# Make sure all folders exist as expected
# @return shinytest runner file location
m__validate_shinytest_exists <- function() {
  if (!fs::dir_exists("tests")) abort("No ./tests directory found")
  shinytest_file <- fs::dir_ls("tests", regexp = "shinytest\\.[rR]$", type = "file")
  if (length(shinytest_file) == 0) abort("No ./tests/shinytest.R file found")
  if (length(shinytest_file) > 1) abort("Multiple files matching `./tests/shinytest` found")
  if (!fs::dir_exists("./tests/shinytest")) abort("./tests/shinytest folder not found")

  # return runner file location
  unname(shinytest_file)
}


m__find_shinytest_testapp <- function(exprs, info_env) {
  is_test_app <- function(expr_list) {
    length(expr_list) >= 1 &&
      is.language(expr_list[[1]]) &&
      st2_expr_text(expr_list[[1]]) %in% c(
        "testApp",
        "shinytest::testApp",
        "shinytest:::testApp"
      )
  }
  args <- NULL
  post_fn <- function(expr_list, is_top_level) {
    if (is_test_app(expr_list)) {
      if (!is.null(args)) {
        if (info_env$verbose) rlang::inform(c(
          "!" = "Multiple shinytest::testApp() calls found. Only the first one will be used."
        ))
      } else {
        args <<- rlang::call_args(
          rlang::call_match(
            as.call(expr_list),
            shinytest::testApp,
            defaults = TRUE
          )
        )
      }
    }
    # Don't alter the expr_list, just return it
    as.call(expr_list)
  }
  # For all exprs, find a single shinytest::testApp()
  lapply(exprs, function(expr) {
    m__recurse_expr(expr, post_fn = post_fn)
  })

  args
}

is_driver_init <- function(expr_list) {
  length(expr_list) >= 1 &&
    is.language(expr_list[[1]]) &&
    st2_expr_text(expr_list[[1]]) %in% c(
      "ShinyDriver$new",
      "shinytest::ShinyDriver$new",
      "shinytest:::ShinyDriver$new"
    )
}
m__find_shinydriver_new <- function(exprs, info_env) {
  is_shinydriver_new_assignment <- function(expr_list) {
    length(expr_list) >= 3 &&
    is.language(expr_list[[1]]) &&
    st2_expr_text(expr_list[[1]]) %in% c("`<-`", "`=`", "`<<-`") &&
    is_driver_init(expr_list[[3]])
  }

  ret <- list()
  post_fn <- function(expr_list, is_top_level) {
    if (is_shinydriver_new_assignment(expr_list)) {
      app_var <- expr_list[[2]]
      new_args <- rlang::call_args(
        rlang::call_match(
          as.call(expr_list),
          shinytest::ShinyDriver$public_methods$initialize,
          defaults = TRUE
        )
      )
      ret <<- append(ret, list(list(app_var = app_var, args = args)))
    }
    # Don't alter the expr_list, just return it
    as.call(expr_list)
  }
  # For all exprs, find a single shinytest::testApp()
  lapply(exprs, function(expr) {
    m__recurse_expr(expr, post_fn = post_fn)
  })

  ret
}


# Extract the runner information, such as `suffix` and a fully populated `testnames`
m__extract_runner_info <- function(app_info_env) {
  shinytest_file <- m__validate_shinytest_exists()
  app_info_env$shinytest_runner_file <- shinytest_file
  exprs <- parse(file = shinytest_file)

  testapp_args <- m__find_shinytest_testapp(exprs, app_info_env)
  if (is.null(testapp_args)) {
    rlang::inform(c(
      "!" = paste0("No `shinytest::testApp()` call found in file: ", shinytest_file),
      "i" = "Using defaults! Assuming `shinytest::testApp(testDir='..')`"
    ))
    testapp_args <- formals(shinytest::testApp)
    testapp_args$appDir <- ".." # nolint
  }
  if (fs::path_rel(testapp_args$appDir) != "..") {
    abort(paste0(
      "shinytest::testApp() must be called on the parent App directory (`appDir = \"..\"`).\n",
      "{shinytest2} does not know how to automatically migrate this app."
    ))
  }
  # Store knowledge
  tryCatch(
    testnames <- eval(testapp_args$testnames, envir = globalenv()),
    error = function(e) {
      rlang::abort("Could not use variables for `testnames` in `shinytest::testApp()`. Only atomic values are supported.")
    }
  )
  app_info_env$testnames <-
    shinytest___find_tests("tests/shinytest", testnames)
  # Eventually always set the variant to the suffix
  # to allow for $expect_screenshot() to work
  app_info_env$suffix <- testapp_args$suffix
  app_info_env$compare_images <- testapp_args$compareImages %||% TRUE
  invisible()
}

# Save a new shinytest2 runner file
m__write_shinytest2_runner <- function(app_info_env) {
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


m__parse_test_files <- function(app_info_env) {
  if (app_info_env$verbose) {
    rlang::inform(c(
      "i" = paste0("`suffix`: '", app_info_env$suffix, "'"),
      "i" = paste0("`compareImages`: ", app_info_env$compare_images),
      "i" = paste0("`testnames`: ", paste0(app_info_env$testnames, collapse = ", "))
    ))
  }

  lapply(app_info_env$testnames, function(testname) {
    test_path <- file.path("tests/shinytest", testname)
    # Reset the environment for the next file by removing flags / prior file knowledge
    info_env <- m__reset_info_env(app_info_env)

    m__parse_test_file(test_path, info_env)
    m__expected_files(test_path, info_env)
  })
}

m__reset_info_env <- function(app_info_env) {
  for (key in c(
    "test_path",
    "save_path",
    "app_var",
    "name",
    "screenshot_snapshot_init",
    "match_found",
    # Not `take_screenshot`; We want this value to persist
    NULL
  )) {
    if (exists(key, envir = app_info_env, inherits = FALSE)) {
      rm(list = key, envir = app_info_env, inherits = FALSE)
    }
  }
  app_info_env
}

m__testthat_chunk <- function(title, body_txts) {

  # TODO-future; better regex! to add indents and trim blank lines
  indented_body_texts <-
    gsub("(^|\n)", "\\1  ", body_txts)
  # Remove tailing whitespace that was just added
  # odd pairs
  indented_body_texts <-
    gsub("\n  \n", "\n\n", indented_body_texts)
  # even pairs
  indented_body_texts <-
    gsub("\n  \n", "\n\n", indented_body_texts)

  paste0(
    "library(shinytest2)\n",
    "\n",
    "test_that(\"", title, "\", {\n",
      paste0(indented_body_texts, collapse = "\n"), "\n",
    "})\n"
  )
}


# `info_env$name` is name of output folder
# `info_env$suffix` is variant used in output folder name
m__expected_files <- function(test_path, info_env) {

  # IDK if EXTRA should be replaced or prepended

  # Find all expected folders in shinytest folder with matching `name` prefix
  # Copy contents
  # * from `tests/shinytest/NAME-expected[-SUFFIX]/XXX.json`
  # * to   `tests/testthat/_snaps/[SUFFIX/]NAME/XXX.json`

  shinytest_dirs <- list.dirs(
    "tests/shinytest",
    full.names = TRUE,
    recursive = FALSE
  )
  shinytest_dirs <- shinytest_dirs[grepl("-expected", fs::path_file(shinytest_dirs), fixed = TRUE)]
  if (info_env$verbose) {
    rlang::inform(c(
      i = "Migrating expected files from `tests/shinytest` to `tests/testthat`"
    ))
  }

  lapply(shinytest_dirs, function(shinytest_dir) {
    if (info_env$verbose) rlang::inform(c("*" = shinytest_dir))
    shinytest_folder <- fs::path_file(shinytest_dir)

    testthat_path <- fs::path("tests", "testthat", "_snaps")
    if (grepl("-expected-", shinytest_folder)) {
      suffix_path <- strsplit(shinytest_folder, "-expected-")[[1]][[2]]
      testthat_path <- fs::path(testthat_path, suffix_path)
    }
    test_name <- strsplit(shinytest_folder, "-expected")[[1]][[1]]
    testthat_path <- fs::path(testthat_path, test_name)
    # Make sure destination exists
    fs::dir_create(testthat_path)

    shinytest_files <- dir(shinytest_dir, full.names = TRUE)
    lapply(seq_along(shinytest_files), function(i) {
      shinytest_file <- shinytest_files[i]

      expected_file <- fs::path_file(shinytest_file)

      cur_number_reg <- regexpr("(?<digits>\\d\\d\\d).(json|png)$", expected_file, perl = TRUE)
      if (cur_number_reg > 0) {
        start <- attr(cur_number_reg, "capture.start")[1, ][["digits"]]
        length <- attr(cur_number_reg, "capture.length")[1, ][["digits"]]
        cur_number_txt <- substr(expected_file, start, start + length - 1)
        cur_number <- as.integer(cur_number_txt)

        new_number <- cur_number * 2
        if (fs::path_ext(expected_file) == "json") {
          new_number <- new_number - 1
        }

        new_number <- as.character(new_number)
        new_number_txt <- paste0(paste0(rep("0", 3 - nchar(new_number)), collapse = ""), new_number)
        # Turn new number into the file name
        expected_file <- sub(paste0(cur_number_txt, "."), paste0(new_number_txt, "."), expected_file, fixed = TRUE)
      }

      fs::file_copy(
        # `tests/shinytest/NAME-expected[-SUFFIX]/XXX.json`
        shinytest_file,
        # `tests/testthat/_snaps/[SUFFIX/]NAME/XXX.json`
        fs::path(
          testthat_path,
          expected_file
        ),
        overwrite = TRUE
      )
    })
  })
}




m__parse_test_file <- function(test_path, info_env) {
  if (info_env$verbose) {
    rlang::inform(c("i" = paste0("Migrating test: ", test_path)))
  }

  info_env$test_path <- test_path
  info_env$from_file <- fs::path_file(test_path)
  info_env$save_path <-
    fs::path("tests", "testthat", paste0("test-", info_env$from_file))

  test_text <- read_utf8(test_path)
  migrated_text <- m__parse_test_text(test_text, test_path, info_env)

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

  if (info_env$verbose) {
    rlang::inform(c(i = paste0("Writing: ", info_env$save_path)))
  }
  testthat_text <- m__testthat_chunk(
    title = paste0("Migrated shinytest test: ", info_env$from_file),
    body_txts = migrated_text
  )
  fs::dir_create(fs::path_dir(info_env$save_path))
  write_utf8(testthat_text, info_env$save_path)
}

m__parse_test_text <- function(test_text, test_path, info_env) {
  test_lines <- strsplit(test_text, "\n")[[1]]

  if (length(test_lines) == 0) {
    return(character(0))
  }

  init_infos <- m__find_shinydriver_new(parse(text = test_text), info_env)
  if (length(init_infos) == 0) abort(paste0("Can not find `ShinyDriver$new` in test file: ", test_path))
  # TODO-future; split the code into parts and recurse
  if (length(init_infos) > 1) abort(paste0("Can not migrate file that contains multiple calls to `ShinyDriver$new`: ", test_path))
  info_env$app_var <- init_infos[[1]]$app_var
  info_env$init_args <- init_infos[[1]]$args


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
  migrated_text <- m__algo_2(test_text, m__shinytest_lang, info_env)
  # migrated_lines <- strsplit(migrated_text, "\n")[[1]]

  # Do a second pass to add back the AppDriver$new() calls.
  txt <- m__algo_2(migrated_text, m__driver_new, info_env)
  return(txt)
}

m__driver_new <- function(expr, info_env) {

  post_fn <- function(expr_list, ...) {
    if (!is_driver_init(expr_list)) {
      return(as.call(expr_list))
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
    # Do not do this if using a unique file name
    # init_args$name <- info_env$name
    init_args$variant <- rlang::maybe_missing(info_env$suffix, NULL)
    # No need to do this as the pictures are different anyways
    # init_args$width <- 992
    # init_args$height <- 744
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
      if (info_env$verbose) rlang::inform("`ShinyDriver$new(debug=)` is not supported by `AppDriver`. All debugging messages are always recorded.")
    }
    if ("phantomTimeout" %in% expr_args_names) {
      if (info_env$verbose) rlang::inform("`ShinyDriver$new(phantomTimeout=)` is not supported by `AppDriver`. `{chromote}` does not have a timeout parameter.")
    }

    ret_expr <- rlang::call2(
      rlang::expr(AppDriver$new),
      !!!init_args
    )

    # Signify that a match was found for m__recurse_expr
    info_env$match_found <- TRUE
    # Return upgraded expr
    ret_expr
  }

  m__recurse_expr(expr, post_fn = post_fn)
}

# Copy over code to avoid being hosed down the road
shinytest___find_tests <- function(tests_dir, testnames = NULL) {
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
m__algo_2 <- function(test_text, expr_fn, info_env) {
  test_lines <- strsplit(test_text, "\n")[[1]]
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
      expr_texts <- for_each_expr_text(
        exprs,
        expr_fn,
        info_env
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



# m__shinytest_lang <- function(expr, info_env, is_top_level = FALSE) {
#   shinytest_lang_is_fn <- function(expr_list) {
#     expr_fn <- expr_list[[1]]

#     is.language(expr_fn) &&
#       length(expr_fn) >= 3 &&
#       expr_fn[[1]] == "$" &&
#       expr_fn[[2]] == info_env$app_var
#   }

#   if (!is.language(expr)) {
#     return(expr)
#   }
#   expr_list <- as.list(expr)

#   if (
#     # Return early if it is a single item
#     length(expr_list) == 1 &&
#     # Make sure not something like `app$getAllValues()`
#     is.language(expr_list[[1]]) &&
#     length(expr_list[[1]]) == 1
#   ) {
#     return(expr)
#   }
#   # # Some methods return a list of values
#   # new_expr_list <- list()
#   # for (expr_list_item in expr_list) {
#   #   new_expr_list <- append(
#   #     new_expr_list,
#   #     m__shinytest_lang(expr_list[[i]], info_env, is_top_level = FALSE)
#   #   )
#   # }
#   for (i in seq_len(length(expr_list))) {
#     expr_list[[i]] <-
#       m__shinytest_lang(expr_list[[i]], info_env, is_top_level = FALSE)
#   }

#   if (!shinytest_lang_is_fn(expr_list)) {
#     return(
#       # Reconstruct language call
#       rlang::call2(expr_list[[1]], !!!expr_list[-1])
#     )
#   }

#   # By being after the for-loop, it alters from the leaf to the trunk
#   # Mark that a match was found
#   info_env$match_found <- TRUE
#   # Match against known function names in expr_list[[3]]
#   matched_expr <- match_shinytest_expr(expr_list, is_top_level, info_env)
#   matched_expr
# }

m__shinytest_lang <- function(expr, info_env) {
  shinytest_lang_is_fn <- function(expr_list) {
    expr_fn <- expr_list[[1]]

    is.language(expr_fn) &&
      length(expr_fn) >= 3 &&
      expr_fn[[1]] == "$" &&
      expr_fn[[2]] == info_env$app_var
  }

  post_fn <- function(expr_list, is_top_level) {
    if (!shinytest_lang_is_fn(expr_list)) {
      return(
        # Reconstruct language call
        rlang::call2(expr_list[[1]], !!!expr_list[-1])
      )
    }

    # Mark that a match was found
    info_env$match_found <- TRUE
    # Match against known function names in expr_list[[3]]
    matched_expr <- match_shinytest_expr(expr_list, is_top_level, info_env)
    matched_expr
  }
  m__recurse_expr(expr, post_fn = post_fn)
}


m__recurse_expr <- function(expr, post_fn) {
  m__recurse_expr_ <- function(expr, post_fn, is_top_level = FALSE) {
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
      expr_list[[i]] <-
        m__recurse_expr_(expr_list[[i]], post_fn, is_top_level = FALSE)
    }

    # By being after the for-loop, it alters from the leaf to the trunk
    post_fn(expr_list, is_top_level)
  }
  # Shim `is_top_level = TRUE`
  m__recurse_expr_(expr = expr, post_fn = post_fn, is_top_level = TRUE)
}






match_shinytest_expr <- function(expr_list, is_top_level, info_env) {
  # message("Found expr!:")
  expr_fn <- expr_list[[1]]
  app_fn_sym <- expr_fn[[3]]

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
  inform_js <- function(fn_name, arg_name, info_env) {
    if (info_env$verbose) rlang::inform(c(
      i = paste0("`ShinyDriver$", fn_name, "(", arg_name, "=)` would automatically return the last value. `AppDriver`'s `ChromoteSession` does not auto return the last value.",
      "x" = "Please add JavaScript `return` statements appropriately.")
    ))
  }

  switch(as.character(app_fn_sym),
    "setInputs" = {
      matched_args <- match_shinytest_args("setInputs")
      matched_args_names <- names(matched_args)
      if ("allowInputNoBinding_" %in% matched_args_names) {
        names(matched_args)[matched_args_names == "allowInputNoBinding_"] <- "allow_no_input_binding_"
      }
      # Yell about removed functionality
      if (
        !is_top_level || isTRUE(matched_args[["values_"]])
      ) {
        rlang::abort("`ShinyDriver$setInputs(values_=)` is no longer supported. Use `AppDriver$get_values()` directly. (This message was thrown because `ShinyDriver$setInputs()`'s result is possibly used.)")
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
      inform_js("executeScript", "script", info_env)

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
        if (info_env$verbose) rlang::inform(c(
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
          prior_output_value <- (!!app_val)$get_value(output = (!!output_val))
          (!!app_val)$set_inputs(!!!input_vals, timeout_ = !!timeout_val)
          new_output_value <- (!!app_val)$get_value(output = (!!output_val))
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
    "findElement" = , # nolint
    "findElements" = , # nolint
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
      if (info_env$verbose) rlang::inform(c(
        i = "`ShinyDriver$enableDebugLogMessages()` is not implemented in `AppDriver`. All debug messages are always recorded in {shinytest2}.",
        "!" = "Removing call to `ShinyDriver$enableDebugLogMessages()`"
      ))
      NULL
    },
    "getDebugLog" = , # nolint
    "getEventLog" = {
      rlang::inform(c(
        i = paste0("`ShinyDriver$", as.character(app_fn_sym), "()` is not implemented in `AppDriver`."),
        "!" = "A single `AppDriver$get_log()` method should be used."
      ))

      shinytest2_expr("get_log", list())
    },

    "getSnapshotDir" = , # nolint
    "getRelativePathToApp" = , # nolint
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
      matched_args <- match_shinytest_args("getValue")
      iotype <- iotype_arg(matched_args, "getValue")
      if (iotype == "auto") {
        if (info_env$verbose) rlang::inform(c(
          "!" = "`ShinyDriver$getValue(iotype=)` is set to `\"auto\"`; Using `output` as a guess."
        ))
        iotype <- "output"
      }
      fn_args <- list()
      fn_args[[iotype]] <- matched_args$name
      app_val <- rlang::sym(info_env$app_var)
      shinytest2_expr("get_value", fn_args)
    },
    "setValue" = {

      matched_args <- match_shinytest_args("setValue")
      iotype <- iotype_arg(matched_args, "setValue")
      if (iotype == "output") {
        rlang::abort(
          "`ShinyDriver$setValue(iotype=)` is set to `\"output\"`; {shinytest2} does not support setting output values directly."
        )
      }
      if (info_env$verbose) rlang::inform(c(
        i = "`ShinyDriver$setValue()` is not implemented in `AppDriver`. It relied on invasive Shiny logic.",
        "!" = "Replacing this with a call to `AppDriver$set_inputs()`"
      ))
      if (iotype == "auto") {
        if (info_env$verbose) rlang::inform(c(
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
      if (info_env$verbose) rlang::inform(c(
        "*" = "`ShinyDriver$snapshotInit()` is not implemented in `AppDriver`.",
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
      # -1. If exists, User preference value
      # 0. If exists, Prior user prompt value
      # 1. FALSE, if `compareImages == FALSE``
      # 2. If exists, `old$snapshot(screenshot=)`
      # 3. If exists, `old$snapshotInit(screenshot=)`
      # 4. User prompt value
      take_screenshot <- local({
        # -1. If exists, User preference value
        if (!is.null(
          rlang::maybe_missing(info_env$include_expect_screenshot, NULL)
        )) {
          return(info_env$include_expect_screenshot)
        }

        # 0. If exists, Prior user prompt value
        if (!is.null(info_env$take_screenshot)) return(info_env$take_screenshot)

        if (is_false(info_env$compare_images)) return(FALSE)
        if (!is.null(matched_args$screenshot)) return(isTRUE(matched_args$screenshot))
        # Could be NULL, FALSE, or TRUE
        if (!is.null(info_env$screenshot_snapshot_init)) return(info_env$screenshot_snapshot_init)

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

      # Yell about removed functionality
      if (
        !is_top_level || isTRUE(matched_args[["values_"]])
      ) {
        rlang::abort("`ShinyDriver$uploadFile(values_=)` is no longer supported. Use `AppDriver$get_values()` directly. (This message was thrown because `ShinyDriver$uploadFile()`'s result is possibly used.)")
      }
      matched_args[["values_"]] <- NULL

      shinytest2_expr("upload_file", matched_args)
    },

    "waitFor" = {
      # wait_for_js
      inform_js("waitFor", "expr", info_env)
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
for_each_expr_text <- function(exprs, expr_fn, ...) {
  unlist(lapply(exprs, function(expr) {
    st2_expr_text(expr_fn(expr, ...))
  }))
}
