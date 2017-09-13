#' Run breathtestshiny app
#'
#' @export
#' @importFrom shiny runApp
run_shiny = function() {
  appDir = system.file("shiny", package = "breathtestshiny")
  if (appDir == "") {
    stop("Could not Shiny app in breathtestshiny", call. = FALSE)
  }
  runApp(appDir, display.mode = "normal")
}
