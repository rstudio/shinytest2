# Note:
# `app_get_values()` retrieves the RDS format of the shiny values.
#   * If `hash_images` is TRUE, then it recurses over the RDS object and hashes all images.
# `app_expect_values()` retrieves the JSON text format of the shiny values.
#   * All images are hashed within the text content.

app_get_single_ioe <- function(
  self,
  private,
  ...,
  input = missing_arg(),
  output = missing_arg(),
  export = missing_arg()
) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  # input <- rlang::maybe_missing(input, FALSE)
  # output <- rlang::maybe_missing(output, FALSE)
  # export <- rlang::maybe_missing(export, FALSE)
  input_is_provided <- !rlang::is_missing(input)
  output_is_provided <- !rlang::is_missing(output)
  export_is_provided <- !rlang::is_missing(export)

  if (sum(input_is_provided, output_is_provided, export_is_provided) != 1) {
    app_abort(
      self,
      private,
      "Must specify only one of `input`, `output`, `export`"
    )
  }

  if (input_is_provided) {
    ckm8_assert_single_string(input)
  } else if (output_is_provided) {
    ckm8_assert_single_string(output)
  } else if (export_is_provided) {
    ckm8_assert_single_string(export)
  } else {
    app_abort(self, private, "Missing ioe type", .internal = TRUE)
  }

  type <-
    if (input_is_provided) {
      "input"
    } else if (output_is_provided) {
      "output"
    } else if (export_is_provided) {
      "export"
    }
  name <-
    if (input_is_provided) {
      input
    } else if (output_is_provided) {
      output
    } else if (export_is_provided) {
      export
    }

  list(
    input = input,
    output = output,
    export = export,
    type = type,
    name = name
  )
}

app_get_value <- function(
  self,
  private,
  ...,
  input = missing_arg(),
  output = missing_arg(),
  export = missing_arg(),
  hash_images = FALSE
) {
  ioe <- app_get_single_ioe(
    self,
    private,
    input = input,
    output = output,
    export = export
  )

  # Call `app_get_values()` to get the RDS format of the shiny values.
  ret <- self$get_values(
    input = ioe$input,
    output = ioe$output,
    export = ioe$export,
    hash_images = hash_images
  )

  # Extract the single value
  ret[[ioe$type]][[ioe$name]]
}


# Note: This queries method the Shiny server
app_get_values <- function(
  self,
  private,
  ...,
  input = missing_arg(),
  output = missing_arg(),
  export = missing_arg(),
  hash_images = FALSE
) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()
  hash_images <- isTRUE(hash_images)

  self$log_message("Getting all values")
  "!DEBUG app_get_values()"

  url <- app_get_shiny_test_url(
    self,
    private,
    input,
    output,
    export,
    format = "rds"
  )

  # Ask Shiny for info
  cur_env <- rlang::current_env()
  req <- app_httr_get(self, private, url, fn_404 = function(req) {
    app_abort(
      self,
      private,
      c(
        paste0("Shiny server returned 404 for values URL: ", url),
        "i" = "Is `shiny::runApp(test.mode = TRUE)` enabled?"
      ),
      call = cur_env
    )
  })

  ## Writing to memory is 2x faster than disk and produces the same result
  ## However, the `disk` approach is tried and tested in `{shinytest}`
  ## `rawConnection()` duplicates the memory and may be bad for large files
  # > bench::mark(
  # + mem = {
  # +   local({
  # +     raw_conn <- gzcon(rawConnection(req$content))
  # +     on.exit({
  # +       if (isOpen(raw_conn)) {
  # +         close(raw_conn)
  # +       }
  # +     }, add = TRUE)
  # +     readRDS(raw_conn)
  # +   })
  # + },
  # + disk = {
  # +   local({
  # +     tmpfile <- tempfile()
  # +     on.exit(unlink(tmpfile), add = TRUE)
  # +     writeBin(req$content, tmpfile)
  # +     readRDS(tmpfile)
  # +   })
  # + }
  # + )
  # # A tibble: 2 × 13
  #   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result           memory  time   gc
  #   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>           <list>  <list> <lis>
  # 1 mem        433.46µs 462.21µs     1989.  143.47KB     2.04   977     1      491ms <named list [3]> <Rprof… <benc… <tib…
  # 2 disk         1.06ms   1.37ms      727.    1.32KB     0      364     0      501ms <named list [3]> <Rprof… <benc… <tib…

  tmpfile <- tempfile()
  on.exit(unlink(tmpfile), add = TRUE)
  writeBin(req$content, tmpfile)
  values <- readRDS(tmpfile)

  if (hash_images) {
    values <- hash_obj_images(values)
  }

  values
}


app_expect_values <- function(
  self,
  private,
  ...,
  input = missing_arg(),
  output = missing_arg(),
  export = missing_arg(),
  name = NULL,
  screenshot_args = missing_arg(),
  transform = NULL
) {
  ckm8_assert_app_driver(self, private)
  rlang::check_dots_empty()

  self$log_message("Expecting values")
  "!DEBUG app_expect_values()"

  screenshot_args_ <- default_screenshot_args(
    maybe_missing_value(
      screenshot_args,
      private$default_expect_values_screenshot_args
    )
  )

  json_path <- app_next_temp_snapshot_path(self, private, name, "json")
  # `NAME.json` -> `NAME_.png`; `NAME_.new.png`
  png_path <-
    fs::path_ext_set(
      paste0(fs::path_ext_remove(json_path), "_"),
      "png"
    )

  # Announce snapshot file before touching before any other expressions can fail
  testthat::local_edition(3)
  testthat::announce_snapshot_file(json_path)
  testthat::announce_snapshot_file(png_path)

  url <- app_get_shiny_test_url(
    self,
    private,
    input,
    output,
    export,
    format = "json"
  )
  # Ask Shiny for info
  cur_env <- rlang::current_env()
  req <- app_httr_get(self, private, url, fn_404 = function(req) {
    app_abort(
      self,
      private,
      c(
        paste0("Shiny server returned 404 for values URL: ", url),
        "i" = "Is `shiny::runApp(test.mode = TRUE)` enabled?"
      ),
      call = cur_env
    )
  })

  # Convert to text, then replace base64-encoded images with hashes.
  content <- raw_to_utf8(req$content)
  # original_content <- content
  content <- hash_snapshot_image_data(content, is_json_file = TRUE)
  # Adjust the text to _pretty_ print
  content <- jsonlite::prettify(content, indent = 2)

  # Take a screenshot for debugging purposes
  # If `screenshot_args_` is anything other than `FALSE`...
  if (!is_false(screenshot_args_)) {
    # If a single output is used, then zoom in on the output
    # TODO-future; Should we do this for `inputs` as well! Works nicely for `outputs`!
    if (length(output) == 1 && is.character(output)) {
      # Define the default selector value, but let the user override this with their `screenshot_args`
      screenshot_args_ <- utils::modifyList(
        list(selector = paste0("#", output, ".shiny-bound-output")),
        screenshot_args_
      )
    }
    withCallingHandlers(
      # swallow expectation
      {
        # Take screenshot using snapshot expectation.
        # Skip the variant check in `$expect_snapshot()`
        # Leverage testthat snapshot logic, but muffle any expectation output
        app_expect_screenshot(
          self,
          private,
          name = png_path,
          screenshot_args = screenshot_args_
        )
      },
      # Muffle any expectation (good or bad) thrown by testthat
      expectation = function(ex) {
        # Continue, skipping the signaling of the condition
        # https://github.com/r-lib/testthat/pull/2271/files#diff-eeb22563925ae9725656cfbfc44bd5001428734041747d5d90d364464e8e651cR107
        invokeRestart("muffle_expectation")
      }
    )
  }

  # Write the json file for comparison purposes
  write_utf8(content, json_path)
  on.exit(unlink(json_path), add = TRUE)
  # Assert json file contents
  app__expect_snapshot_file(
    self,
    private,
    json_path,
    transform = transform,
    compare = testthat::compare_file_text
  )

  invisible(content)
}


app_get_shiny_test_url <- function(
  self,
  private,
  input = missing_arg(),
  output = missing_arg(),
  export = missing_arg(),
  format = "json"
) {
  ckm8_assert_app_driver(self, private)

  input_is_missing <- rlang::is_missing(input)
  output_is_missing <- rlang::is_missing(output)
  export_is_missing <- rlang::is_missing(export)
  if (sum(input_is_missing, output_is_missing, export_is_missing) == 3) {
    # If nothing is supplied, use all values
    input <- output <- export <- TRUE
  } else {
    # If something is supplied, disable all other values
    input <- rlang::maybe_missing(input, FALSE)
    output <- rlang::maybe_missing(output, FALSE)
    export <- rlang::maybe_missing(export, FALSE)
  }

  q_string <- function(group, value) {
    if (isTRUE(value)) {
      paste0(group, "=1")
    } else if (is.character(value)) {
      paste0(group, "=", paste(value, collapse = ","))
    } else {
      ""
    }
  }
  paste(
    private$shiny_test_url,
    q_string("input", input),
    q_string("output", output),
    q_string("export", export),
    paste0("format=", format),
    "sortC=1",
    sep = "&"
  )
}


# Given a JSON string, find any strings that represent base64-encoded images
# and replace them with a hash of the value. The image is base64-decoded and
# then hashed with SHA1. The resulting hash value is the same as if the image
# were saved to a file on disk and then hashed.
hash_snapshot_image_data <- function(
  data,
  is_json_file = TRUE
) {
  # Search for base64-encoded image data. There are two named groups:
  # - data_url is the entire data URL: "data:image/png;base64," and the base64-encoded data
  # - img_data is just the base64-encoded data.
  pattern <-
    if (is_json_file) {
      # Trailing quotes are added back at the end of the function.
      '\\n\\s*"[^"]*"\\s*:\\s*"(?<data_url>data:image/[^;]+;base64,(?<img_data>[^"]+))"'
    } else {
      '^(?<data_url>data:image/[^;]+;base64,(?<img_data>[^"]+))$'
    }

  image_offsets <- gregexpr(pattern, data, perl = TRUE)[[1]]

  # No image data found
  if (length(image_offsets) == 1 && image_offsets == -1) {
    return(data)
  }

  attr2 <- function(x, name) {
    attr(x, name, exact = TRUE)
  }

  # Image data indices
  image_start_idx <- as.integer(attr2(image_offsets, "capture.start")[,
    "img_data"
  ])
  image_stop_idx <- image_start_idx +
    as.integer(attr2(image_offsets, "capture.length")[, "img_data"]) -
    1

  # Text (non-image) data indices
  text_start_idx <- c(
    0,
    attr2(image_offsets, "capture.start")[, "data_url"] +
      attr2(image_offsets, "capture.length")[, "data_url"]
  )
  text_stop_idx <- c(
    attr(image_offsets, "capture.start")[, "data_url"] - 1,
    nchar(data)
  )

  # Get the strings representing image data, and all the other stuff
  image_data <- substring(data, image_start_idx, image_stop_idx)
  text_data <- substring(data, text_start_idx, text_stop_idx)
  # Hash the images
  image_hashes <- vapply(image_data, FUN.VALUE = "", function(dat) {
    tryCatch(
      {
        image_data <- jsonlite::base64_dec(dat)
        rlang::hash(
          image_data
        )
      },
      error = function(e) {
        "Error hashing image data"
      }
    )
  })

  image_hashes <- paste0("[image data hash: ", image_hashes, "]")

  # There's one fewer image hash than text elements. We need to add a blank
  # so that we can properly interleave them.
  image_hashes <- c(image_hashes, "")

  # Interleave the text data and the image hashes
  paste(
    c(rbind(text_data, image_hashes)),
    collapse = ""
  )
}


hash_obj_images <- function(obj) {
  if (length(obj) == 0) {
    return(obj)
  }
  # if (is.character(obj)) return(hash_base64_image(obj))
  if (is.character(obj)) {
    return(hash_snapshot_image_data(obj, is_json_file = FALSE))
  }
  if (is.list(obj)) {
    return(lapply(obj, hash_obj_images))
  }
  obj
}
