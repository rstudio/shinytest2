skip_if_no_apps <- function() {
  if (!dir.exists(test_path("apps/hello"))) {
    skip(paste0("App test folder has been ignored"))
  }
}

skip_if_no_local_apps <- function() {
  if (!dir.exists(test_path("local-apps/window"))) {
    skip(paste0("Local app test folder has been ignored"))
  }
}
