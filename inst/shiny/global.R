library(shiny)
library(stringr)
data_root = "~/breathtestcore/"

cleanup_uid = function(uid){
  uid = str_replace_all(tolower(uid), " ","_")
  paste0(unlist(str_extract_all(uid, "[a-z0-9_]*")), collapse = "")
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


ace_options = "ace.edit('data').setOptions({tabSize:12,showInvisibles:true,useSoftTabs:false});"
