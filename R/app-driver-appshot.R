



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
  screenshot_args <- screenshot_args %||% private$default_screenshot_args %||% (!is.null(items))
  should_take_screenshot <- !is_false(screenshot_args)

  if (items_is_false) {
    if (!should_take_screenshot) {
      # TODO-barret; Fix this
      if (is.null(private$default_screenshot_args)) browser()
      abort("Both 'items' and 'screenshot_args' can not be `FALSE` at the same time.", app = self)
    }
  }

  snapshot_count <- private$appshot_count$increment()
  temp_save_dir  <- private$appshot_dir
  create_snapshot_dir(temp_save_dir, snapshot_count)

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
    full_json_path <- fs::path(temp_save_dir, json_name)
    write_utf8(content, full_json_path)
  }

  full_screenshot_path <- NULL
  if (should_take_screenshot) {
    # Replace extension with .png
    full_screenshot_path <- fs::path(temp_save_dir, fs::path_ext_set(json_name, "png"))
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


create_snapshot_dir <- function(dir, count) {
  if (count == 1) {
    if (fs::dir_exists(dir)) {
      unlink(dir, recursive = TRUE)
    }
    dir.create(dir, recursive = TRUE)
  }
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
    rlang::with_handlers({
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
