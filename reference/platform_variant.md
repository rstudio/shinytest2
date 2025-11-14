# Platform specific variant

Returns a string to be used within testthat's' snapshot testing.
Currently, the Operating System and R Version (major, minor, no patch
version) are returned.

## Usage

``` r
platform_variant(..., os_name = TRUE, r_version = TRUE)
```

## Arguments

- ...:

  Must be empty. Allows for parameter expansion.

- os_name:

  if `TRUE`, include the OS name in the output

- r_version:

  if `TRUE`, include the major and minor version of the R version, no
  patch version

## Details

If more information is needed in the future to distinguish standard
testing environments, this function will be updated accordingly.

## See also

[`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
