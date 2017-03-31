library(shiny)
if (is.null(unlist(options("shiny.port"))))
  options(shiny.port = 34712)
base_url = paste0("localhost:",options("shiny.port"))

