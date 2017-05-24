#' Run breathtestshiny app
#'
#' @return  Not used, starts shiny app
#' @export
#'
run_shiny = function() {
  appDir = system.file("shiny", package = "breathtestshiny")
  if (appDir == "") {
    stop("Could not Shiny app in breathtestshiny", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
