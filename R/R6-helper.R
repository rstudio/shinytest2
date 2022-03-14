
Count <- R6Class( # nolint
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
  self, private,
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


Url <- R6Class( # nolint
  "Url",
  private = list(
    url = NULL
  ),
  public = list(
    get = function() {
      private$url
    },
    set = function(url) {
      res <- httr::parse_url(url)

      checkmate::assert_subset(res$scheme, c("http", "https"), .var.name = "url scheme")

      if (!is.null(res$port)) {
        res$port <- as.integer(res$port)
        ckm8_assert_single_integer(res$port, .var.name = "url port")
      }

      ckm8_assert_single_string(res$hostname, .var.name = "url hostname")

      private$url <- httr::build_url(res)

      invisible(self)
    }
  )
)
