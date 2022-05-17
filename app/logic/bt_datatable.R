box::use(
  dplyr[`%>%`, across, mutate]
)

#' @export
bt_datatable = function(cf){
  if (is.null(cf) || nrow(cf) == 0)
    return(NULL)
  search_options = list(paging = FALSE, searching = TRUE,
                        autoWidth = TRUE,
                        search = list(search = "t50"),
                        dom = 'Bfrtip',
                        buttons = c('copy', 'excel', 'pdf'))
  # Converting to factor is required for data table smart filtering
  cm = comment(cf)
  cf = cf %>%
    mutate(across(where(is.character), as.factor))
  DT::datatable(cf, rownames = FALSE, caption = cm,
                extensions  = 'Buttons',
                filter = "top",
                options = search_options)
}
