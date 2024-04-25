#' @param variant
#'   If not-`NULL`, results will be saved in
#'   `_snaps/{variant}/{test.md}`, so `variant` must be a single
#'   string of alphanumeric characters suitable for use as a
#'   directory name.
#'
#'   You can variants to deal with cases where the snapshot output
#'   varies and you want to capture and test the variations.
#'   Common use cases include variations for operating system, R
#'   version, or version of key dependency. For example usage,
#'   see [`platform_variant()`].
