#' Remote control a Shiny app running in a headless browser
#'
#' @description
#' This class starts a Shiny app in a new R session, along with a \pkg{chromote}
#' headless browser that can be used to simulate user actions. This provides
#' a full simulation of a Shiny app so that you can test user interactions
#' with a live app.
#'
#' @param iotype Type of the Shiny widget. Usually shinytest2 finds the widgets
#'   by their name, so this is only needed if you use the same name for an
#'   input and output widget.
#' @param timeout,timeout_ Amount of time to wait before giving up (milliseconds).
#' @param wait_ Wait until all reactive updates have completed?
#' @param name Name of a shiny widget.
#' @param css CSS selector to find an HTML element.
#' @param linkText Find `<a>` HTML elements based on exact `innerText`
#' @param partialLinkText Find `<a>` HTML elements based on partial `innerText`
#' @param xpath Find HTML elements using XPath expressions.
#' @param checkInterval How often to check for the condition, in ms.
#' @importFrom R6 R6Class
#' @export
ShinyDriver2 <- R6Class(
  "ShinyDriver2",

  # public = rlang::list2(




  #   # #' @description Deprecated.
  #   # #' @param ... Ignored
  #   # snapshotCompare = function(...) {
  #   #   # TODO-barret
  #   #   abort("app$snapshotCompare() is no longer used")
  #   # }
  # )

)
