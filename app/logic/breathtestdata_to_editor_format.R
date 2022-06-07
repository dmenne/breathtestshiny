
#' @export
breathtestdata_to_editor_format = function(data, data_subset=NULL) {
  data_head = utils::head(data) # For example display
  data = breathtestcore::cleanup_data(data)
  tc = textConnection("dt", "w", local = TRUE)
  comment = comment(data)
  if (!is.null(data_subset))
    writeLines(paste0("# ", paste0(data_subset, collapse = ", ")), con = tc)
  suppressWarnings(utils::write.table(data, file = tc, append = TRUE,
                               row.names = FALSE, sep = "\t", quote = FALSE))
  dt_ed = paste(dt, collapse = "\n")
  close(tc)
  attr(dt_ed, "data_head") = data_head
  dt_ed
}
