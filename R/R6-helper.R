# nolint start
Count <- R6Class(
  # nolint end
  "Count",
  private = list(
    count = 0
  ),
  public = list(
    increment = function() {
      private$count <- private$count + 1
      private$count
    },
    get = function() {
      private$count
    }
  )
)

app_next_temp_snapshot_path <- function(
  self,
  private,
  name, # full path or filename
  ext = "json"
) {
  ckm8_assert_app_driver(self, private)

  fs::path(
    private$save_dir,
    # set the file extension
    fs::path_ext_set(
      # take file name only
      fs::path_file(
        name %||% sprintf("%03d", private$counter$increment())
      ),
      ext
    )
  )
}


# nolint start
Url <- R6Class(
  # nolint end
  "Url",
  private = list(
    url = NULL
  ),
  public = list(
    get = function() {
      private$url
    },
    # Combine the stored URL with a relative sub-path, properly handling
    # query parameters on the base URL. (#357)
    combine = function(sub_url) {
      base_parsed <- httr2::url_parse(private$url)
      base_query <- base_parsed$query
      base_parsed$query <- NULL
      full_url <- paste0(httr2::url_build(base_parsed), sub_url)
      if (length(base_query) > 0) {
        full_parsed <- httr2::url_parse(full_url)
        full_parsed$query <- c(full_parsed$query, base_query)
        full_url <- httr2::url_build(full_parsed)
      }
      full_url
    },
    set = function(url) {
      res <- tryCatch(
        httr2::url_parse(url),
        error = function(e) {
          # httr2::url_parse() uses curl underneath which is stricter
          # Convert parsing errors to validation errors
          if (grepl("Port number", e$message)) {
            warning(e$message)
            stop("Assertion on 'url port' failed")
          }
          if (grepl("parse URL", e$message)) {
            stop("Assertion on 'url hostname' failed: Must be a single string")
          }
          stop(e)
        }
      )

      checkmate::assert_subset(
        res$scheme,
        c("http", "https"),
        .var.name = "url scheme"
      )

      # Legacy curl behavior: missing path is "/";
      # Tested:
      # * Broken in {curl} v5.2.1; path is `NULL`
      # * Work with {curl} v7; path is `/`
      if (is.null(res$path)) {
        res$path <- "/"
      }

      if (!is.null(res$port)) {
        res$port <- as.integer(res$port)
        ckm8_assert_single_integer(res$port, .var.name = "url port")
      }

      ckm8_assert_single_string(res$hostname, .var.name = "url hostname")

      private$url <- httr2::url_build(res)

      invisible(self)
    }
  )
)
