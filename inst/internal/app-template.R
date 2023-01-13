library(shiny)

`_data` <- readRDS("data.rds") # nolint

lapply(`_data`$packages, library, character.only = TRUE)
for (prefix in names(`_data`$resources)) {
  shiny::addResourcePath(prefix, `_data`$resources[[prefix]])
}

# ## Great for debugging
# cat("server env (before):\n")
# print(pryr::rls(environment(`_data`$server)))

# Make globals environment and shim it in between server environment
# and original server parent environment as to not affect the server environment
server_parent_env <- parent.env(environment(`_data`$server))

# cat("making globals environment: ", paste0(names(`_data`$globals), collapse = ""), "\n", sep = "")
app_globals_env <- list2env(`_data`$globals, envir = NULL) # make a new environment

# Shim globals env in between server environment and original server parent environment

# Must be first; Don't want to disconnect the global env if run second
parent.env(app_globals_env) <- server_parent_env
# Must be second
parent.env(environment(`_data`$server)) <- app_globals_env

# ## Great for debugging
# cat("globals env:\n")
# print(pryr::rls(app_globals_env))
# cat("server env:\n")
# print(pryr::rls(environment(`_data`$server)))

shinyApp(`_data`$ui, `_data`$server)
