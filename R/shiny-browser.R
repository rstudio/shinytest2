with_external_shiny_browser <- function(code) {
  ## Logic

  ## If Chrome is available, open in Chrome, except if RStudio server.
  ## RStudio desktop can not open in: window, pane. Only external
  ## RStudio server can open in any viewer location: window, pane, external.
  ##   Do not adjust settings

  ## Therefore,
  ## If not RStudio server
  ##   If Chrome, open in Chrome
  ##   else If RStudio desktop, open in external

  is_avail <- rstudio_is_available()
  rstudio_mode <- is_avail && rstudioapi::versionInfo()$mode
  is_rstudio_server <- is_avail && rstudio_mode == "server"
  is_rstudio_desktop <- is_avail && rstudio_mode != "server"

  if (!is_rstudio_server) {
    # Match the same Chromote object when initializing AppDriver's Chromote session
    # Use similar logic to `browse_url()`
    browser <- chromote::default_chromote_object()$get_browser()
    if (inherits(browser, "Chrome")) {
      # Has local Chrome browser!
      # Do not mess with the IDE options, instead just overwrite the
      # shiny.launch.browser option

      # Use the local Chrome browser path and shell escape it
      browser_path <- shQuote(browser$get_path())
      open_chrome <- function(url) {
        utils::browseURL(url, browser_path)
      }
      old_options <- options(shiny.launch.browser = open_chrome)
      withr::defer({
        options(old_options)
      })
    } else if (is_rstudio_desktop) {
      # Local RStudio desktop!

      # Force the url to open in the external browser
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
  }

  force(code)
}
