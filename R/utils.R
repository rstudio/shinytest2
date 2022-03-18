
# nolint start
ckm8_assert_single_string <- function(x, .var.name = checkmate::vname(x)) {
  checkmate::assert_character(x, len = 1, any.missing = FALSE, .var.name = .var.name, null.ok = FALSE)
}
ckm8_assert_single_integer <- function(x, ..., len = 1, any.missing = FALSE, .var.name = checkmate::vname(x)) {
  checkmate::assert_integer(x, len = len, any.missing = any.missing, .var.name = .var.name, ...)
}
ckm8_assert_single_number <- function(x, ..., .var.name = checkmate::vname(x)) {
  checkmate::assert_number(x, .var.name = .var.name, ...)
}
ckm8_assert_app_driver <- function(self, private, self.var.name = checkmate::vname(self), private.var.name = checkmate::vname(private)) {
  checkmate::assert_r6(self, "AppDriver", .var.name = self.var.name)
  checkmate::assert_environment(private, .var.name = private.var.name)
}
# nolint end

# Cache a value given output of `fn`
cache_fn_val <- function(fn) {
  val <- NULL
  function() {
    if (!is.null(val)) return(val)

    val <<- fn()
    val
  }
}

on_ci <- function() {
 isTRUE(as.logical(Sys.getenv("CI")))
}

raw_to_utf8 <- function(data) {
  res <- rawToChar(data)
  Encoding(res) <- "UTF-8"
  res
}

read_raw <- function(file) {
  readBin(file, "raw", n = file.info(file)$size)
}

read_utf8 <- function(file) {
  res <- read_raw(file)
  raw_to_utf8(res)
}

# write text as UTF-8
write_utf8 <- function(text, ...) {
  writeBin(charToRaw(enc2utf8(text)), ...)
}



# nolint start
# https://github.com/rstudio/shiny/blob/2360bde13efac1fe501efee447a8f3dde0136722/R/shiny.R#L35-L49
toJSON <- function(x, ...,  dataframe = "columns", null = "null", na = "null",
  auto_unbox = TRUE, digits = getOption("shiny.json.digits", 16),
  use_signif = TRUE, force = TRUE, POSIXt = "ISO8601", UTC = TRUE,
  rownames = FALSE, keep_vec_names = TRUE, strict_atomic = TRUE) {

  if (strict_atomic) {
    x <- I(x)
  }

  # I(x) is so that length-1 atomic vectors get put in [].
  jsonlite::toJSON(x, dataframe = dataframe, null = null, na = na,
   auto_unbox = auto_unbox, digits = digits, use_signif = use_signif,
   force = force, POSIXt = POSIXt, UTC = UTC, rownames = rownames,
   keep_vec_names = keep_vec_names, json_verbatim = TRUE, ...)
}
toJSON_atomic <- function(x, ...) {
  toJSON(x, ..., strict_atomic = FALSE)
}
# nolint end




# For PhantomJS on Windows, the pHYs (Physical pixel dimensions) header enbeds
# the computer screen's actual resolution, even though the screenshots are
# done on a headless browser, and the actual screen resolution has no effect
# on the pixel-for-pixel content of the screenshot.
#
# The header can differ when expected results are generated on one computer
# and compared to results from another computer, and this causes shinytest to
# report false positives in changes to screenshots. In order to avoid this
# problem, this function rewrites the pHYs header to always report a 72 ppi
# resolution.
#
# https://github.com/ariya/phantomjs/issues/10659#issuecomment-14993827
normalize_png_res_header <- function(self, private, file) {
  data <- readBin(file, raw(), n = 512)
  header_offset <- grepRaw("pHYs", data)

  if (length(header_offset) == 0) {
    app_warn(self, private, paste0("Cannot find pHYs header in ", fs::path_file(file)))
    return(FALSE)
  }

  # Replace with header specifying 2835 pixels per meter (equivalent to 72
  # ppi).
  con <- file(file, open = "r+b")
  seek(con, header_offset - 1, rw = "write")
  writeBin(png_res_header_data, con)
  close(con)

  return(TRUE)
}

png_res_header_data <- as.raw(c(
  0x70, 0x48, 0x59, 0x73,  # "pHYs"
  0x00, 0x00, 0x0b, 0x13,  # Pixels per unit, X: 2835
  0x00, 0x00, 0x0b, 0x13,  # Pixels per unit, Y: 2835
  0x01,                    # Unit specifier: meters
  0x00, 0x9a, 0x9c, 0x18   # Checksum
))

app_inform_where <- function(self, private, message) {
  ckm8_assert_app_driver(self, private)

  bt <- rlang::trace_back(bottom = parent.frame())
  bt_string <- paste0(format(bt), collapse = "\n")

  app_inform(self, private, paste0(message, "\n", bt_string))
}


# Sort items using the C locale, which is used with `method = "radix"`
sort_c <- function(x) {
  if (length(x)) {
    sort(x, method = "radix")
  } else {
    x
  }
}

st2_temp_file <- function(fileext = "", pattern = "") {
  tempfile(pattern = paste0("st2-", pattern), fileext = fileext)
}


is_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
