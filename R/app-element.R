# TODO-barret; should these methods be included? Or should the methods be moved into chromote?

#' @description
#' Find an HTML element on the page using a CSS selector. If multiple elements are matched, only the first is returned.
#' @param css CSS selector to find an HTML element.
# ' @param linkText Find `<a>` HTML elements based on exact `innerText`
# ' @param partialLinkText Find `<a>` HTML elements based on partial `innerText`
# ' @param xpath Find HTML elements using XPath expressions.
#' @return An NodeId to the element
ShinyDriver2$set("public", "findElement", function(css = NULL
) {
  "!DEBUG ShinyDriver2$findElement '`css`'"
  # private$web$findElement(css, linkText, partialLinkText, xpath)
  chromote_find_element(private$chromote_obj, css)
})

#' @description
#' Find all elements matching CSS selection.
# ' @return A list of [webdriver::Element]s.
#' @return An list of NodeId corresponding to the matching elements.
ShinyDriver2$set("public", "findElements", function(css = NULL
# , linkText = NULL, partialLinkText = NULL, xpath = NULL
) {
  "!DEBUG ShinyDriver2$findElements '`css`'"
  chromote_find_elements(private$chromote_obj, css)
})
