with_external_shiny_browser <- function(code) {
  ## Logic
  # if Chrome and not RStudio server, open with chrome
  # else if RStudio is available, set RStudio prefs
  # else (not Chrome and no RStudio), do nothing

  is_rstudio_server <- rstudio_is_available() && rstudioapi::versionInfo()$mode == "server"

  # Match the same Chromote object when initializing AppDriver's Chromote session
  browser <- chromote::default_chromote_object()$get_browser()
  if ((!is_rstudio_server) && inherits(browser, "Chrome")) {
    # Local! (or not RStudio server!)
    # Do not mess with the IDE options, instead just overwrite the
    # shiny.launch.browser option

    # If Chrome is locally available, use the local Chrome browser
    browser_path <- shQuote(browser$get_path())
    open_chrome <- function(url) {
      browseURL(url, browser_path)
    }
    old_options <- options(shiny.launch.browser = open_chrome)
    withr::defer({
      options(old_options)
    })
  } else if (rstudio_is_available()) {
    # Server!

    # If in the IDE, force the url to open in the external browser
    # Inspriation: https://github.com/rstudio/shinycoreci/blob/fa634714f14df8130cb60a6a1c8f07f53603a27b/R/test_in_ide.R#L98-L145
    if (rstudioapi::isAvailable("1.3.387")) {
      # See https://docs.rstudio.com/ide/server-pro/1.3.1073-1/session-user-settings.html
      # user, none, pane, window, browser; Default is 'window'
      shiny_viewer_type <- rstudioapi::readRStudioPreference("shiny_viewer_type", "window")
      # Restore option
      withr::defer({
        rstudioapi::writeRStudioPreference("shiny_viewer_type", shiny_viewer_type)
      })

      # Force the url to open in the external browser
      rstudioapi::writeRStudioPreference("shiny_viewer_type", "browser")
    } else {
      # RStudio, but early version
      open_external <- get(".rs.invokeShinyWindowExternal", envir = as.environment("tools:rstudio"))
      old_option <- options(shiny.launch.browser = open_external)
      withr::defer({
        options(old_option)
      })

    }
  }

  force(code)
}
