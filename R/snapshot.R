sd2_snapshot <- function(
  self, private,
  items = NULL,
  name = NULL,
  screenshot = NULL
) {
  if (!is.list(items) && !is.null(items))
    abort("'items' must be NULL or a list.")
  private$snapshotCount <- private$snapshotCount + 1

  current_dir  <- self$getSnapshotDir()

  # Do not prefix with `self$name` as that is only necessary for the snapshot file name
  # At this point, the temp folder is already unique
  json_name <- name %||% sprintf("%03d.json", private$snapshotCount)

  # The default is to take a screenshot when the snapshotScreenshot option is
  # TRUE and the user does not specify specific items to snapshot.
  if (is.null(screenshot)) {
    screenshot <- private$snapshotScreenshot && is.null(items)
  }

  # Figure out which items to snapshot ----------------------------------------
  # By default, record all items.
  if (is.null(items)) {
    items <- list(input = TRUE, output = TRUE, export = TRUE)
  }

  extra_names <- setdiff(names(items), c("input", "output", "export"))
  if (length(extra_names) > 0) {
    abort(paste0(
      "'items' must be a list containing one or more items named",
      "'input', 'output' and 'export'. Each of these can be TRUE, FALSE, ",
      " or a character vector."
    ))
  }

  if (is.null(items$input))  items$input  <- FALSE
  if (is.null(items$output)) items$output <- FALSE
  if (is.null(items$export)) items$export <- FALSE

  # Take snapshot -------------------------------------------------------------
  self$logEvent("Taking snapshot")
  url <- private$getTestSnapshotUrl(items$input, items$output, items$export)
  req <- httr_get(url)

  # For first snapshot, create -current snapshot dir.
  if (private$snapshotCount == 1) {
    if (isTRUE(dir.exists(current_dir))) {
      unlink(current_dir, recursive = TRUE)
    }
    dir.create(current_dir, recursive = TRUE)
  }

  # Convert to text, then replace base64-encoded images with hashes of them.
  original_content <- content <- raw_to_utf8(req$content)
  content <- hash_snapshot_image_data(content)
  # utils::str(content)
  # TODO-barret; turn into alpha sorted lists; insert logic here! https://github.com/rstudio/shinytest/issues/409
  conent <- sort_second_level_keys(content)
  content <- jsonlite::prettify(content, indent = 2)
  full_json_path <- fs::path(current_dir, json_name)
  write_utf8(content, full_json_path)

  full_screenshot_path <- NULL
  if (screenshot) {
    # Replace extension with .png
    full_screenshot_path <- fs::path(current_dir, fs::path_ext_set(json_name, "png"))
    self$takeScreenshot(full_screenshot_path)
  }

  list(
    screenshot_path = full_screenshot_path,
    json_path = full_json_path,
    json_original_content = original_content,
    json_content = content
  )
  # TODO-prior;?; Invisibly return JSON content as a string
  # invisible(original_content)
}


sd2_getTestSnapshotUrl <- function(
  self, private,
  input, output,
  export,
  format
) {
  reqString <- function(group, value) {
    if (isTRUE(value))
      paste0(group, "=1")
    else if (is.character(value))
      paste0(group, "=", paste(value, collapse = ","))
    else
      ""
  }
  paste(
    private$shinyTest2SnapshotBaseUrl,
    reqString("input", input),
    reqString("output", output),
    reqString("export", export),
    paste0("format=", format),
    sep = "&"
  )
}


sort_second_level_keys <- function(x) {
  lapply(x, function(items) {
    if (length(items) == 0) return(items)

    # get names and sort keys for consistent ordering
    items_names <- names(items)
    items_names <- sort(items_names)
    items[items_names]
  })
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
  image_start_idx <- as.integer(attr2(image_offsets, "capture.start")[,"img_data"])
  image_stop_idx <- image_start_idx +
    as.integer(attr2(image_offsets, "capture.length")[,"img_data"]) - 1

  # Text (non-image) data indices
  text_start_idx <- c(
    0,
    attr2(image_offsets, "capture.start")[,"data_url"] +
      attr2(image_offsets, "capture.length")[,"data_url"]
  )
  text_stop_idx <- c(
    attr(image_offsets, "capture.start")[,"data_url"] - 1,
    nchar(data)
  )

  # Get the strings representing image data, and all the other stuff
  image_data <- substring(data, image_start_idx, image_stop_idx)
  text_data  <- substring(data, text_start_idx,  text_stop_idx)

  # Hash the images
  image_hashes <- vapply(image_data, FUN.VALUE = "", function(dat) {
    tryCatch({
      image_data <- jsonlite::base64_dec(dat)
      digest::digest(
        image_data,
        algo = "sha1", serialize = FALSE
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
