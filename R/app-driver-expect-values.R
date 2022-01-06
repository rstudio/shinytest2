# TODO-barret; use rds. Add note about complex objects may have serialization issues.
# TODO-barret; look into using a `file()` to read `req$content` and not use a `rawConnection()` as it is "slow"


# Note: This queries method the Shiny server
app_get_values <- function(
  self, private,
  input = TRUE,
  output = TRUE,
  export = TRUE,
  ...,
  hash_images = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()
  hash_images <- isTRUE(hash_images)

  self$log_message("Getting all values")
  "!DEBUG app_get_values()"

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

  url <- app_get_shiny_test_url(self, private, input, output, export, format = "rds")
  # Ask shiny for info
  req <- httr_get(url)

  ## Writing to memory is 2x faster than disk and produces the same result
  ## However, the `disk` approach is tried and tested in `{shinytest}`
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

  raw_conn <- gzcon(rawConnection(req$content))
  on.exit({
    if (isOpen(raw_conn)) {
      close(raw_conn)
    }
  }, add = TRUE)
  values <- readRDS(raw_conn)

  if (hash_images) {
    values <- hash_obj_images(values)
  }

  values
}

app_expect_values <- function(
  self, private,
  ...,
  input = missing_arg(),
  output = missing_arg(),
  export = missing_arg(),
  name = NULL,
  screenshot = missing_arg(),
  hash_images = TRUE,
  cran = FALSE
) {
  ckm8_assert_app_driver(self, private)
  ellipsis::check_dots_empty()

  self$log_message("Expecting values")
  "!DEBUG app_expect_values()"

  # TODO-barret; handle if a screenshot should be taken
  # TODO-barret; handle what screenshot args should be used? `screenshot` -> `self$default_screenshot_args` -> list()
  # TODO-barret; Should the screenshot "zoom" if an single output is used? Should this functionality be used in `app$expect_screenshot()?
  screenshot_is_missing <- rlang::is_missing(screenshot)
  screenshot <- rlang::maybe_missing(screenshot, self$values_screenshot)

  # Do not prefix with `self$name` as that is only necessary for the snapshot file name
  # At this point, the temp folder is already unique
  # Only increment the counter if counter is used
  json_path <- app_next_temp_snapshot_path(self, private, name, "json")

  # Capture values
  values <- self$get_values(
    input = input,
    output = output,
    export = export,
    hash_images = hash_images
  )

  # Take a screenshot for debugging purposes
  # TODO-barret; handle screenshot arg;
  if (TRUE || should_take_screenshot) {
    withCallingHandlers(
      {
        # `NAME.json` -> `NAME_.png`
        png_path <-
          fs::path_ext_set(
            paste0(fs::path_ext_remove(json_path), "_"),
            "png"
          )
        # Take screenshot using snapshot expectation.
        # Leverage testthat snapshot logic, but muffle any expectation output
        app_expect_screenshot( # TODO convert to self fn
          self, private,
          name = png_path,
          screenshot_args = rlang::maybe_missing(screenshot, list())
        )
      },
      # Muffle any expectation
      expectation = function(ex) {
        # Continue, skipping the signaling of the condition
        # https://github.com/r-lib/testthat/blob/38c087d3bb5ec3c098c181f1e58a55c687268fba/R/expectation.R#L32-L34
        invokeRestart("continue_test")
      }
    )

  }

  # Write the json file
  write_json(values, json_path)
  # Assert json file contents
  testthat_expect_snapshot_file(
    private,
    json_path,
    cran = cran,
    compare = testthat::compare_file_text
  )

  invisible(values)
}


app_appshot <- function(
  self, private,
  ...,
  items = NULL,
  name = NULL,
  screenshot_args = NULL
) {
  ckm8_assert_app_driver(self, private)
  # The default is to take a screenshot when the `default_screenshot_args` option is
  # NULL and the user does not specify specific items to snapshot.
  items_is_false <- is_false(items)
  # TODO-barret; Fix this
  screenshot_args <- screenshot_args %||% private$default_screenshot_args %||% (!is.null(items))
  should_take_screenshot <- !is_false(screenshot_args)

  if (items_is_false) {
    if (!should_take_screenshot) {
      # TODO-barret; Fix this
      if (is.null(private$default_screenshot_args)) browser()
      abort("Both 'items' and 'screenshot_args' can not be `FALSE` at the same time.", app = self)
    }
  }

  snapshot_count <- private$counter$increment()

  # Do not prefix with `self$name` as that is only necessary for the snapshot file name
  # At this point, the temp folder is already unique
  json_name <- fs::path_ext_set(name %||% sprintf("%03d", snapshot_count), "json")

  full_json_path <- NULL
  if (!items_is_false) {

    # Figure out which items to snapshot ----------------------------------------
    # By default, record all items.
    if (is.null(items) || isTRUE(items)) {
      items <- list(input = TRUE, output = TRUE, export = TRUE)
    }
    if (!is.list(items)) {
      abort("`items` must be TRUE, FALSE, NULL, or a list.", app = self)
    }

    extra_names <- setdiff(names(items), c("input", "output", "export"))
    if (length(extra_names) > 0) {
      abort(paste0(
        "'items' must be a list containing one or more items named",
        "'input', 'output' and 'export'. Each of these can be TRUE, FALSE, ",
        " or a character vector."
      ), app = self)
    }

    if (is.null(items$input))  items$input  <- FALSE
    if (is.null(items$output)) items$output <- FALSE
    if (is.null(items$export)) items$export <- FALSE

    # Take appshot -------------------------------------------------------------
    self$log_message("Taking appshot")
    self$log_message("Gathering input/output/export values")
    url <- app_get_shiny_test_url(self, private, items$input, items$output, items$export)
    req <- httr_get(url)

    # Convert to text, then replace base64-encoded images with hashes of them.
    content <- raw_to_utf8(req$content)
    # original_content <- content
    content <- hash_snapshot_image_data(content)
    content <- jsonlite::prettify(content, indent = 2)
    full_json_path <- fs::path(private$save_dir, json_name)
    write_utf8(content, full_json_path)
  }

  full_screenshot_path <- NULL
  if (should_take_screenshot) {
    # Replace extension with .png
    full_screenshot_path <- fs::path(private$save_dir, fs::path_ext_set(json_name, "png"))
    # Take screenshot
    app_screenshot(self, private, filename = full_screenshot_path, screenshot_args = screenshot_args)
  }

  list(
    screenshot_path = full_screenshot_path,
    json_path = full_json_path
    # json_original_content = original_content,
    # json_content = content
  )
}



app_get_shiny_test_url <- function(
  self, private,
  input = TRUE,
  output = TRUE,
  export = TRUE,
  format = "json"
) {
  ckm8_assert_app_driver(self, private)

  q_string <- function(group, value) {
    if (isTRUE(value))
      paste0(group, "=1")
    else if (is.character(value))
      paste0(group, "=", paste(value, collapse = ","))
    else
      ""
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
hash_snapshot_image_data <- function(data) {

  # Search for base64-encoded image data. There are two named groups:
  # - data_url is the entire data URL, including the leading quote,
  #   "data:image/png;base64,", the base64-encoded data, and the trailing quote.
  # - img_data is just the base64-encoded data.
  image_offsets <- gregexpr(
    '\\n\\s*"[^"]*"\\s*:\\s*(?<data_url>"data:image/[^;]+;base64,(?<img_data>[^"]+)")',
    data,
    perl = TRUE
  )[[1]]

  # No image data found
  if (length(image_offsets) == 1 && image_offsets == -1) {
    return(data)
  }

  attr2 <- function(x, name) {
    attr(x, name, exact = TRUE)
  }

  # Image data indices
  image_start_idx <- as.integer(attr2(image_offsets, "capture.start")[, "img_data"])
  image_stop_idx <- image_start_idx +
    as.integer(attr2(image_offsets, "capture.length")[, "img_data"]) - 1

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
  text_data  <- substring(data, text_start_idx,  text_stop_idx)

  # Hash the images
  image_hashes <- vapply(image_data, FUN.VALUE = "", function(dat) {
    withCallingHandlers({
      image_data <- jsonlite::base64_dec(dat)
      rlang::hash(
        image_data
      )
    }, error = function(e) {
      "Error hashing image data"
    })
  })

  image_hashes <- paste0('"[image data sha1: ', image_hashes, ']"')

  # There's one fewer image hash than text elements. We need to add a blank
  # so that we can properly interleave them.
  image_hashes <- c(image_hashes, "")

  # Interleave the text data and the image hashes
  paste(
    c(rbind(text_data, image_hashes)),
    collapse = ""
  )
}


## Given a JSON string, find any strings that represent base64-encoded images
## and replace them with a hash of the value. The image is base64-decoded and
## then hashed with SHA1. The resulting hash value is the same as if the image
## were saved to a file on disk and then hashed.
hash_base64_image <- function(txt) {
  if (!is.character(txt)) return(txt)
  if (nchar(txt) < 20) return(txt)
  first_txt <- substr(txt, 0, 15)
  # if not a data:image string, return early
  if (!grep("data:image/", first_txt, fixed = TRUE)) return(txt)

  # Search for base64-encoded image data. There are is a named group:
  # - img_data is just the base64-encoded data.
  img_offset <- regexpr(
    'data:image/[^;]+;base64,(?<img_data>[^"]+)',
    txt,
    perl = TRUE
  )

  # No image data found
  if (length(img_offset) == 1 && img_offset == -1) {
    return(txt)
  }

  attr2 <- function(x, name) {
    attr(x, name, exact = TRUE)
  }

  # Image data indices
  image_start_idx <- as.integer(attr2(img_offset, "capture.start")[, "img_data"])
  image_stop_idx <- image_start_idx +
    as.integer(attr2(img_offset, "capture.length")[, "img_data"]) - 1

  # Text (non-image) data indices
  text_start_idx <- 0
  text_stop_idx <- image_start_idx - 1

  # Get the strings representing image data, and all the other stuff
  image_data <- substr(txt, image_start_idx, image_stop_idx)
  text_data  <- substr(txt, text_start_idx,  text_stop_idx)

  # Hash the image
  image_hash <-
    withCallingHandlers({
      rlang::hash(
        jsonlite::base64_dec(image_data)
      )
    }, error = function(e) {
      "Error hashing image data"
    })

  paste0(text_data, '"[image data sha1: ', image_hash, ']"')
}
# Recursive function to iterate over a list to hash all base-64 images found
hash_obj_images <- function(obj) {
  if (length(obj) == 0) return(obj)
  if (is.character(obj)) return(hash_base64_image(obj))
  if (is.list(obj)) return(lapply(obj, hash_obj_images))
  obj
}
