library(shiny)
library(stringr)
library(breathtestcore)
library(breathteststan)
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

popit = function(session, show, id, title, content, placement = "top" ){
  if (show) {
    cat("\n--", content, "--\n", sep = "")
    addPopover(session, id, title, pop_content[content], placement)
  } else {
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

sample_data = function(td){
  data("usz_13c", envir = environment())
  data = usz_13c  %>%
  filter(patient_id == "norm_001", group == "liquid_normal") %>%
  mutate(
    pdr = round(pdr, 1)
  ) %>%
  select(minute, pdr)
  tc = textConnection("dt", "w")
  write.table(data, file = tc, col.names = td == "with_header",
            row.names = FALSE, sep = "\t", quote = FALSE)
  dt = paste(dt, collapse = "\n")
  close(tc)
  dt
}

ace_options =
  "ace.edit('data').setOptions({tabSize:16,showInvisibles:true,useSoftTabs:false,useElasticTabStops:true});"

pop_content = c(
  data_only = "<code>No fit, data only</code> Select this when you add new data or when you change data in the editor. With one of the other options, any change will trigger lengthy calculations.",
  nls = "<code>Individual curve fit (nls)</code> This is the method almost exclusively used in publications. It works for well-behaved breath test samples from healthy volunteers. For pathological records and when the recording time is too short compared to gastric emptying time, it can fail or give highly erratic half-emptying times. Do not use this method for studies when there are multiple records to compare.",
  nlme = "<code>Mixed-model fit (nlme)</code>This method only works for multiple records, which stabilize each other by borrowing strength; it can fail when many extreme records are included. It gives very stable results even for moderate outliers, and is the recommended method for studies if it converges.",
  stan = "<code>Bayesian fit (Stan)</code>Most flexible method, but slow. It can be used with single curves, but really shows with multiple curves from clinical studies. May need a few minutes to give accurate results; for a fast overview, select fewer iterations in the box below.",
    patient_test_data = "<code>Sample test data</code>, <br>13C records from University Hospital of Zürichz collection (usz_13c). Data from healthy volunteers (normals) are pairs from a cross-over study, for a liquid and a solid meal. <Easy> patient data can be fit by individual curves fits. <Difficult> patient data do not give valid results with individual fits, may converge using the nlme population fits, and do converge with Bayesian methods. ",
  no_header = "Single records with two columns for minute and pdr without header",
  with_header = "Single records with two columns and headers <code>minute</code> and <code>pdr</code>",
  two_patient = "With multiple records, the first column must have the header <code>patient_id</code>, followed by <code>minute</code> and <code>pdr</code>",
  cross_over = "When there are multiple records for one patient such as in cross-over studies, the second column must be \code{group} to label the treatment. Group labels must not contain spaces.")

