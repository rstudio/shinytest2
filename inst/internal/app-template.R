library(shiny)

`_data` <- readRDS("data.rds") # nolint

lapply(`_data`$packages, library, character.only = TRUE)
for (prefix in names(`_data`$resources)) {
  shiny::addResourcePath(prefix, `_data`$resources[[prefix]])
}

# cat("attaching globals: ", paste0(names(`_data`$globals), collapse = ", "), "\n", sep = "")

# Assign to global environment so that everything is available to each other
# `list2env(x, envir=globalenv())` is equivalent to multiple calls to `assign(names(x)[[i]], x[[i]], envir = globalenv())`
list2env(`_data`$globals, envir = globalenv()) # nolint

shinyApp(`_data`$ui, `_data`$server)
