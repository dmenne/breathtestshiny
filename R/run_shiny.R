#' Run breathtestshiny app
#' @param port Port for runApp, default 3838
#' @export
#' @importFrom shiny runApp
run_shiny = function(port = 3838) {
  appDir = system.file("shiny", package = "breathtestshiny")
  if (appDir == "") {
    stop("Could not Shiny app in breathtestshiny", call. = FALSE)
  }
  runApp(appDir, port = port, display.mode = "normal")
}
