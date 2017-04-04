library(shiny)
library(stringr)

data_root = "~/breathtestcore/"

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



ace_options = "ace.edit('data').setOptions({tabSize:12,showInvisibles:true,useSoftTabs:false});"

pop_content = c(
  method_a = "<code>linexp</code>, <b>Methode</b><br>Beschreibung ")
