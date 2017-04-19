library(shiny)
library(stringr)
library(breathtestcore)
library(breathteststan)
library(shinyBS)
#options(shiny.error = browser)
data_root = "~/breathtestcore/"
options(shiny.reactlog=TRUE)


cleanup_uid = function(uid){
  if (is.null(uid) || uid == '') return(NULL)
  uid = str_replace_all(str_trim(tolower(uid)), " ","_")
  uid = paste0(unlist(str_extract_all(uid, "[a-z0-9_]*")), collapse = "")
  if (uid == "") return(NULL)
  uid
}

safe_dir_create <- function(path)
{
  dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) &
    isdir
  dt = !dirTest(path)
  if (dt && !dir.create(path))
    stop(gettextf("cannot create directory '%s'", path),
         domain = NA)
  dt
}

popit = function(session, input,  id, title, placement = "right" ){
  content = as.character(pop_content[input[[id]]])
  if (input$show_pop   && !is.na(content)) {
    addPopover(session, id, title, content, placement)
  }
  else {
    removePopover(session, id)
  }
}


patient_test_data = function(td){
  data("usz_13c", envir = environment())
  data = usz_13c  %>%
    filter(patient_id %in% td) %>%
    mutate(
      pdr = round(pdr, 1)
    )
  tc = textConnection("dt", "w")
  #comment = str_replace_all(comment(data),"\\n", " ")
  comment = "Subset of USZ 13C data"
  writeLines(paste0("# ", comment), con = tc)
  writeLines(paste0("# ", paste0(td, collapse = ", ")), con = tc)
  suppressWarnings(write.table(data, file = tc, append = TRUE,
                               row.names = FALSE, sep = "\t", quote = FALSE))
  dt = paste(dt, collapse = "\n")
  close(tc)
  dt
}

sample_data = function(td) {
  data("usz_13c", envir = environment())
  if (td %in% c("no_header", "with_header")) {
    data = usz_13c  %>%
      filter(patient_id == "norm_001", group == "liquid_normal") %>%
      select(minute, pdr)
  } else if (td == "two_patients") {
    data = usz_13c  %>%
      filter(patient_id %in% c("pat_001", "pat_002")) %>%
      select(patient_id, minute, pdr)
  } else if  (td == "cross_over") {
    data = usz_13c  %>%
      filter(patient_id == "norm_007") %>%
           select(patient_id, group, minute, pdr)
  }
  data$pdr = round(data$pdr,1)
  tc = textConnection("dt", "w")
  write.table(data, file = tc, col.names = td != "no_header",
            row.names = FALSE, sep = "\t", quote = FALSE)
  dt = paste(dt, collapse = "\n")
  close(tc)
  dt
}

# Format data from editor into a data frame
format_data = function(data){
  # Replace multiple spaces or tabs by single tab
  data = str_replace_all(data,"([\t ]+)","\t")
  data = str_replace_all(data,",",".")
  if (nchar(data) < 10) return(NULL)
  tc = textConnection(data)
  d = na.omit(read.table(tc, sep = "\t", header = TRUE))
  close(tc)
  comment = paste(unlist(str_extract_all(data, "^#.*\\n")), collapse = "\n")
  comment = str_replace_all(comment,"\\t", " ")
  comment(d) = comment
  d = cleanup_data(d)
  d
}




ace_options =
  "ace.edit('data').setOptions({tabSize:16,showInvisibles:true,useSoftTabs:false,useElasticTabStops:true});"

pop_content = c(
  data_only = "<code>No fit, data only</code> Select this before adding data or when changing data in the editor. With the others options, any edit triggers lengthy calculations.",
  nls = "<code>Individual curve fit (nls)</code> This is the method almost exclusively used in publications. It works for well-behaved breath test samples from healthy volunteers. For pathological records and when the recording time is too short compared to gastric emptying time, it can fail or give highly erratic results. Always use one of the population-based methods for clinical studies where multiple records are to be compared.",
  nlme = "<code>Mixed-model fit (nlme)</code>This method only works for multiple records that stabilize each other by borrowing strength. It can fail when many extreme records are included. Results are very stable even in presence of moderate outliers. If the algorithm converges, this is the recommended method for studies.",
  stan = "<code>Bayesian fit (Stan)</code>Most flexible method, but slow. It can be used with single curves, but really shows with multiple curves from clinical studies. May need a few minutes to give accurate results; for a fast overview, select fewer iterations in the box below.",
  patient_test_data = "<code>Sample test data</code>, <br>13C records from University Hospital of Zürichz collection (usz_13c). Data from healthy volunteers (normals) are pairs from a cross-over study, for a liquid and a solid meal. <Easy> patient data can be fit by individual curves fits. <Difficult> patient data do not give valid results with individual fits, may converge using the nlme population fits, and do converge with Bayesian methods. ",
  no_header = "Single records with two columns for minute and pdr without header",
  with_header = "Single records with two columns and headers <code>minute</code> and <code>pdr</code>",
  two_patients = "With multiple records, the first column must have the header <code>patient_id</code>, followed by <code>minute</code> and <code>pdr</code>",
  cross_over = "When there are multiple records for one patient such as in cross-over studies, the second column must be labeled <code>group</code>. Group labels must not contain spaces.")

