make vignette of steps
make an automatic conversion function?
  keep if >90% success

talk to lionel about snapshots

issue in testthat for keeping snapshots if they are not touched
  https://github.com/r-lib/testthat/issues/1143

automate platform / R version suffix

gha
  auto commit results
  upload an artifact with updated snapshots
    like fix_all_branches, it must be initiated manually, but _automated_


https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L99-L102
  # Announce snapshot file before touching `fig` in case evaluation
  # causes an error. This allows testthat to restore the files
  # (see r-lib/testthat#1393).
  testthat::announce_snapshot_file(name = file)


handle failures:
https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L127-L151
push_log stores information to a log file that is visible by cran and the testthat fail output logs

provide custom function to compare the files:
https://github.com/r-lib/vdiffr/blob/fc03e91cccac04baa875063513b630d80c02e197/R/expect-doppelganger.R#L132


use the annouce_snapshot_file w/ the `suffix` so that they other files are not deleted. Do not announce the main file?
