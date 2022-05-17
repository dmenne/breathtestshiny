#' @export

clear_search_text = function(id){
  box::use(shiny[span, helpText, HTML, actionButton, icon])
  span(
    helpText(HTML("Clear search box below to see<br> all coefficients)"),
             actionButton(paste0(id, "_button"), label = "Info", icon = icon("info"))),
    class = "help-block-right")
}

