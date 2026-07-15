# CI runners -- especially Windows GitHub Actions -- can be slow to start the
# background R process, launch Chrome, connect the Shiny session, and let the
# app become idle. The 15s default `load_timeout` (and 4s default `timeout`)
# lead to flaky "Shiny app did not become stable" / "Timed out waiting for
# JavaScript script to return `true`" errors that vary from run to run.
#
# Give the package's own test suite generous headroom on CI. This only raises
# the ceiling before an app init is declared failed; fast inits are unaffected.
#
# Use environment variables (not `options()`) so the higher timeouts are also
# inherited by the child R processes launched via `callr::rscript()` (e.g. the
# `test-save-app.R` scripts). Only set values that were not already provided.
if (on_ci()) {
  if (!nzchar(Sys.getenv("SHINYTEST2_LOAD_TIMEOUT"))) {
    Sys.setenv(SHINYTEST2_LOAD_TIMEOUT = 60 * 1000) # 60s (default 15s)
  }
  if (!nzchar(Sys.getenv("SHINYTEST2_TIMEOUT"))) {
    Sys.setenv(SHINYTEST2_TIMEOUT = 20 * 1000) # 20s (default 4s)
  }
}
