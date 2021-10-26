# TODO-barret; should Element methods be included? Or should the main logic be moved into chromote?

#' @description
#' Find an HTML element on the page using a CSS selector. If multiple elements are matched, only the first is returned.
#' @param css CSS selector to find an HTML element.
# ' @param linkText Find `<a>` HTML elements based on exact `innerText`
# ' @param partialLinkText Find `<a>` HTML elements based on partial `innerText`
# ' @param xpath Find HTML elements using XPath expressions.
#' @return An NodeId to the element
ShinyDriver2$set("public", "find_element", function(css = NULL
) {
  "!DEBUG ShinyDriver2$find_element '`css`'"
  # private$web$find_element(css, linkText, partialLinkText, xpath)
  chromote_find_element(self$get_chromote_session(), css)
})
# app_find_element <- function(app, css) {
#   chromote::chromote_find_element(self$get_chromote_session(), css)
# }

#' @description
#' Find all elements matching CSS selection.
# ' @return A list of [webdriver::Element]s.
#' @return An list of NodeId corresponding to the matching elements.
ShinyDriver2$set("public", "find_elements", function(css = NULL
# , linkText = NULL, partialLinkText = NULL, xpath = NULL
) {
  "!DEBUG ShinyDriver2$find_elements '`css`'"
  chromote_find_elements(self$get_chromote_session(), css)
})
