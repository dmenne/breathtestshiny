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

popit = function(session, show, id, title, placement = "right" ){
  if (show) {
    content = pop_content[id]
    if (is.na(pop_content[id]))
      content = ""
    addPopover(session, id, title, content, placement)
  } else {
    removePopover(session, id)
  }
}


test_data = function(td){
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

ace_options =
  "ace.edit('data').setOptions({tabSize:16,showInvisibles:true,useSoftTabs:false,useElasticTabStops:true});"

pop_content = c(
  method_a = "<code>linexp</code>, <b>Methode</b><br>Beschreibung ",
  test_data = "<code>Anton</code>, <b>Use test data from USZ collection"
)

