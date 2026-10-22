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

skip_if_chrome_older_than <- function(app, major) {
  product <- app$get_chromote_session()$Browser$getVersion()$product
  version <- as.integer(sub("^[^/]*/([0-9]+).*", "\\1", product))
  if (version < major) {
    skip(paste0("Chrome ", version, " is older than ", major))
  }
}
