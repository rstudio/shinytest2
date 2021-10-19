# @staticimports pkg:staticimports
#  is_installed


# nolint start
ckm8_assert_single_string <- function(x, .var.name = checkmate::vname(x)) {
  checkmate::assert_character(x, len = 1, any.missing = FALSE, .var.name = .var.name)
}
ckm8_assert_single_integer <- function(x, .var.name = checkmate::vname(x)) {
  checkmate::assert_integer(x, len = 1, any.missing = FALSE, .var.name = .var.name)
}
ckm8_assert_single_url <- function(x, .var.name = checkmate::vname(x)) {
  checkmate::assert_character(x, pattern = "^/", len = 1, any.missing = FALSE, .var.name = .var.name)
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

is_all_named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
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


parse_url <- function(url) {
  res <- regexpr("^(?<protocol>https?)://(?<host>[^:/]+)(:(?<port>\\d+))?(?<path>/.*)?$", url, perl = TRUE)

  if (res == -1) abort(paste0(url, " is not a valid URL."))

  start  <- attr(res, "capture.start",  exact = TRUE)[1, ]
  length <- attr(res, "capture.length", exact = TRUE)[1, ]

  get_piece <- function(n) {
    if (start[[n]] == 0) return("")

    substring(url, start[[n]], start[[n]] + length[[n]] - 1)
  }

  list(
    protocol = get_piece("protocol"),
    host     = get_piece("host"),
    port     = get_piece("port"),
    path     = get_piece("path")
  )
}
is_rmd <- function(path) {
  if (utils::file_test("-d", path)) {
    FALSE
  } else if (grepl("\\.Rmd", path, ignore.case = TRUE)) {
    TRUE
  } else {
    FALSE
  }
}

is_app <- function(path) {
  tryCatch(
    {
      shiny::shinyAppDir(path)
      TRUE
    },
    # shiny::shinyAppDir() throws a classed exception when path isn't a
    # directory, or it doesn't contain an app.R (or server.R) file
    # https://github.com/rstudio/shiny/blob/a60406a/R/shinyapp.R#L116-L119
    invalidShinyAppDir = function(x) FALSE,
    # If we get some other error, it's probably from sourcing
    # of the app file(s), so throw that error now
    error = function(x) abort(conditionMessage(x))
  )
}

app_path <- function(path, arg = "path") {
  # must also check for dir (windows trailing '/')
  if (!(file.exists(path) || dir.exists(path))) {
    stop(paste0("'", path, "' doesn't exist"), call. = FALSE)
  }

  if (is_app(path)) {
    app <- path
    dir <- path
  } else if (is_rmd(path)) {
    # Fallback for old behaviour
    if (length(dir(dirname(path), pattern = "\\.[Rr]md$")) > 1) {
      abort("For testing, only one .Rmd file is allowed per directory.")
    }
    app <- path
    dir <- dirname(path)
  } else {
    rmds <- dir(path, pattern = "\\.Rmd$", full.names = TRUE)
    if (length(rmds) != 1) {
      abort(paste0(
        "`", arg, "` doesn't contain 'app.R', 'server.R', or exactly one '.Rmd'"
      ))
    } else {
      app <- rmds
      dir <- dirname(app)
    }
  }

  list(app = app, dir = dir)
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
normalize_png_res_header <- function(file) {
  data <- readBin(file, raw(), n = 512)
  header_offset <- grepRaw("pHYs", data)

  if (length(header_offset) == 0) {
    warning("Cannot find pHYs header in ", fs::path_file(file))
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

inform_where <- function(message) {
  bt <- rlang::trace_back(bottom = parent.frame())
  bt_string <- paste0(format(bt), collapse = "\n")

  rlang::inform(paste0(message, "\n", bt_string))
}


# Sort items using the C locale, which is used with `method = "radix"`
sort_c <- function(x) {
  sort(x, method = "radix")
}
