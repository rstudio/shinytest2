library(shiny)

`_data` <- readRDS("data.rds") # nolint

lapply(`_data`$packages, library, character.only = TRUE)
for (prefix in names(`_data`$resources)) {
  shiny::addResourcePath(prefix, `_data`$resources[[prefix]])
}
for (name in names(`_data`$globals)) {
  message("setting global name: ", name)
  assign(name, `_data`$globals[[name]], environment(`_data`$server))
}

shinyApp(`_data`$ui, `_data`$server)
