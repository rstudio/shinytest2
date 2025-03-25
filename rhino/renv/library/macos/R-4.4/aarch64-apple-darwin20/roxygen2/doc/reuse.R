## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#' Trigonometric approximations
#' @param x Input, in radians.
#' @name trig
NULL

#' @rdname trig
#' @export
sin_ish <- function(x) x - x^3 / 6

#' @rdname trig
#' @export
cos_ish <- function(x) 1 - x^2 / 2

#' @rdname trig
#' @export
tan_ish <- function(x) x + x^3 / 3

## -----------------------------------------------------------------------------
#' Logarithms
#' 
#' @param x A numeric vector
#' @export
log <- function(x, base) ...

#' @rdname log
#' @export
log2 <- function(x) log(x, 2)

#' @rdname log
#' @export
ln <- function(x) log(x, exp(1))

## -----------------------------------------------------------------------------
#' @rdname arith
#' @order 2
add <- function(x, y) x + y

#' @rdname arith
#' @order 1
times <- function(x, y) x * y

## -----------------------------------------------------------------------------
#' @param .data A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables, or
#'   functions of variables. Use [desc()] to sort a variable in descending
#'   order.
arrange <- function(.data, ...) {}

## -----------------------------------------------------------------------------
#' @inheritParams arrange
mutate <- function(.data, ...) {}

#' @inheritParams arrange
summarise <- function(.data, ...) {}

## -----------------------------------------------------------------------------
#' @inheritParams arrange
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs.
#'   The name gives the name of the column in the output.
#'
#'   The value can be:
#'
#'   * A vector of length 1, which will be recycled to the correct length.
#'   * A vector the same length as the current group (or the whole data frame
#'     if ungrouped).
#'   * `NULL`, to remove the column.
#'   * A data frame or tibble, to create multiple columns in the output.
mutate <- function(.data, ...) {}

## -----------------------------------------------------------------------------
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble), or
#'   lazy data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.

## ----include = FALSE----------------------------------------------------------
roxygen2:::markdown_on()

simple_inline <- "#' Title `r 1 + 1`
#'
#' Description `r 2 + 2`
foo <- function() NULL
"

## ----code=simple_inline-------------------------------------------------------
#' Title `r 1 + 1`
#'
#' Description `r 2 + 2`
foo <- function() NULL


## ----code = roxygen2:::markdown(simple_inline)--------------------------------
#' Title 2
#'
#' Description 4
foo <- function() NULL

## -----------------------------------------------------------------------------
alphabet <- function(n) {
  paste0("`", letters[1:n], "`", collapse = ", ")
}

## ----echo=FALSE---------------------------------------------------------------
env <- new.env()
env$alphabet <- alphabet
roxygen2:::roxy_meta_set("evalenv", env)

backtick <- "#' Title
#' 
#' @param x A string. Must be one of `r alphabet(5)`
foo <- function(x) NULL
"

## ----code = backtick----------------------------------------------------------
#' Title
#' 
#' @param x A string. Must be one of `r alphabet(5)`
foo <- function(x) NULL


## ----code = roxygen2:::markdown_pass1(backtick)-------------------------------
#' Title
#' 
#' @param x A string. Must be one of `a`, `b`, `c`, `d`, `e`
foo <- function(x) NULL


