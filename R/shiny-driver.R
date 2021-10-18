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
#' @importFrom R6 R6Class
#' @export
ShinyDriver2 <- R6Class("ShinyDriver2") # nolint
