app_save <- function(app, path = fs::file_temp("st2-")) {
  rlang::check_installed("globals")

  fs::dir_create(path, recurse = TRUE)
  fs::file_copy(
    system.file("internal/app-template.R", package = "shinytest2"),
    fs::path(path, "app.R")
  )

  data <- app_data(app)
  saveRDS(data, fs::path(path, "data.rds"))

  path
}

# Open questions:
# * what happen if app uses non-exported function?
app_data <- function(app) {
  server <- app$serverFuncSource()
  globals <- app_server_globals(server)

  data <- globals$globals
  data$ui <- environment(app$httpHandler)$ui
  # If the app is made from shinyAppDir with a wrapper around the handler...
  # See https://github.com/rstudio/shiny/blob/fd7518018cbb2ec0b89960c922e34576d5dbc1e7/R/shinyapp.R#L407
  if (is.null(data$ui)) {
    try({
      first_handler <- environment(app$httpHandler)$handlers[[1]]
      data$ui <- environment(environment(first_handler)$appObj()$httpHandler)$ui
    }, silent = TRUE)
  }
  data$server <- server
  data$resources <- shiny::resourcePaths()
  data$packages <- globals$packages
  data
}

app_server_globals <- function(server) {
  # https://github.com/HenrikBengtsson/globals/issues/61#issuecomment-731777640
  rlang::check_installed("globals", version = "0.14.0")

  globals <- globals::globalsOf(server, envir = environment(server), recursive = FALSE)
  globals <- globals::cleanup(globals)

  # remove globals found in packages
  pkgs <- globals::packagesOf(globals)
  in_package <- vapply(
    attr(globals, "where"),
    function(x) !is.null(attr(x, "name")),
    logical(1)
  )
  globals <- globals[!in_package]
  attributes(globals) <- list(names = names(globals))

  list(
    globals = globals,
    packages = pkgs
  )
}
