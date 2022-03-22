#' Test reporter: unique test names per file
#'
#' @description
#' Given a single test file, all snapshot file names should be unique
#'
#' @noRd
get_unique_name_reporter <- function() {
  # Wrap in a function to avoid binding to a static version of `testthat::Reporter`

  UniqueNameReporter <- R6Class("UniqueNameReporter",
    inherit = testthat::ProgressReporter,
    public = list(
      seen = list(),
      cur_file = NULL,

      reset_seen = function() {
        message("restting seen!")
        self$seen <- list()
      },

      initialize = function(...) {
        super$initialize(...)

        self$reset_seen()
      },

      start_file = function(file) {
        message("starting file: ", file)
        self$cur_file <- file
        self$reset_seen()
      }
    )
  )

  UniqueNameReporter$new()
}


#' @export
UniqueNameReporter <- NULL

# Would need to keep `seen` in the package scope to be able to check for files in the package at expectation time
# Would cripple possible parallel support
# Overall seems very hacky


set_unique_name_reporter_on_load <- function() {
  UniqueNameReporter <<-
    R6Class("UniqueNameReporter",
      inherit = testthat::Reporter,
      public = list(
        seen = list(),
        cur_file = NULL,

        reset_seen = function() {
          message("resetting seen!")
          self$seen <- list()
        },

        initialize = function(...) {
          super$initialize(...)

          self$reset_seen()
        },

        start_file = function(file) {
          message("starting file: ", file)
          self$cur_file <- file
          self$reset_seen()
        },
        add_result = function(context, test, result) {
          str(list(
            context = context,
            test = test,
            result = result
          ))
        }
      )
    )
}
