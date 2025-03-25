## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## -----------------------------------------------------------------------------
#' Add two numbers together
#' 
#' @param x,y A pair of numbers.
#' @export
add <- function(x, y) {
  x + y
}

## -----------------------------------------------------------------------------
#' Take an object to bizarro world
#' 
#' @param x A vector.
#' @export
bizarro <- function(x, ...) {
  UseMethod("bizarro")
}

## -----------------------------------------------------------------------------
#' @export
bizarro.character <- function(x, ...) {
  letters <- strsplit(x, "")
  letters_rev <- lapply(letters, rev)
  vapply(letters_rev, paste, collapse = "", FUN.VALUE = character(1))
}

## -----------------------------------------------------------------------------
#' Take an object to bizarro world
#' 
#' @description
#' This is an S3 generic. This package provides methods for the 
#' following classes:
#' 
#' * `character`: reverses the order of the letters in each element of 
#'    the vector.
#' 
#' @param x A vector.
#' @export
bizarro <- function(x, ...) {
  UseMethod("bizarro")
}

#' @export
#' @rdname bizarro
bizarro.character <- function(x, ...) {
  letters <- strsplit(x, "")
  letters_rev <- lapply(letters, rev)
  vapply(letters_rev, paste, collapse = "", FUN.VALUE = character(1))
}

## -----------------------------------------------------------------------------
#' @exportS3Method pkg::generic
generic.foo <- function(x, ...) {
}

## -----------------------------------------------------------------------------
# From dplyr:
#' @rawNamespace import(vctrs, except = data_frame)

# From backports:
#' @rawNamespace if (getRversion() < "4.0.0") export(stopifnot)

## -----------------------------------------------------------------------------
#' @importFrom pkg fun1 fun2
#' @importFrom pkg2 fun3
#' @importFrom pkg3 fun4
NULL

