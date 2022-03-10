if (!require("pak")) {
  options(pak.no_extra_messages = TRUE)
  install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable/")
}
pak::pkg_install("rstudio/shiny")
pak::pkg_install("rstudio/shinyvalidate")
