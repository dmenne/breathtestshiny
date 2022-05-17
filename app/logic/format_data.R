#' @export
# Format data from editor into a data frame
format_data = function(data){
  box::use(stringr[str_replace_all, str_extract_all])
  # Replace multiple spaces or tabs by single tab
  data = str_replace_all(data,"([\t ]+)","\t")
  data = str_replace_all(data,",",".")
  if (nchar(data) < 10) return(NULL)
  tc = textConnection(data, local = TRUE)
  d = stats::na.omit(utils::read.table(tc, sep = "\t", header = TRUE))
  close(tc)
  comment = paste(unlist(str_extract_all(data, "^#.*\\n")), collapse = "\n")
  comment = str_replace_all(comment,"\\t", " ")
  comment = str_replace_all(comment,"^# *", "")
  comment(d) = comment
  breathtestcore::cleanup_data(d)
}

