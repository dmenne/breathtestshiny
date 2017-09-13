suppressPackageStartupMessages(library(shiny))
library(stringr)
suppressPackageStartupMessages(library(dplyr))
library(breathtestcore)
library(breathteststan)
library(shinyBS)
#options(shiny.error = browser)
data_root = "~/breathtestcore"
options(shiny.reactlog = TRUE)
options(shiny.part = 4141)
options(digits = 3) # used in signif
ncol_facetwrap = 4 # for facet_wrap, number of columns
# Behaviour of plot with 2 chains is strange
chains = min(parallel::detectCores(logical = TRUE), 2)

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

pop_select = function(session, input,  id, title, placement = "right" ){
  content = as.character(pop_content[input[[id]]])
  if (is.na(content))
    content = ""
  if (input$show_pop ) {
    addPopover(session, id, title, content, placement)
  }
  else {
    removePopover(session, id)
  }
}

pop_control = function(session, input,  id, title, placement = "right" ) {
  if (input$show_pop) {
    addPopover(session, id, title, pop_content[[id]])
  } else {
    removePopover(session, id)
  }
}

breathtestdata_to_editor_format = function(data, data_subset=NULL){
  data = cleanup_data(data)
  tc = textConnection("dt", "w")
  comment = comment(data)
  if (!is.null(data_subset))
    writeLines(paste0("# ", paste0(data_subset, collapse = ", ")), con = tc)
  suppressWarnings(write.table(data, file = tc, append = TRUE,
                               row.names = FALSE, sep = "\t", quote = FALSE))
  dt = paste(dt, collapse = "\n")
  close(tc)
  dt
}

get_simulated_data = function(data_subset){
  # Retrieves simulated data
  if (is.null(data_subset) | data_subset == "") return(NULL)
  cov = # empirical covariance
    structure(c(188, -0.026, -2.04, -0.026, 7.74e-06, 0.000477, -2.04, 0.000477, 0.182),
              .Dim = c(3L, 3L), .Dimnames = list(c("m", "k","beta"), c("m", "k", "beta")))
  if (data_subset == "nice_1") {
    data = simulate_breathtest_data(7,  cov = cov, k_mean = 0.015)
  } else if (data_subset == "nice_cross") {
    data = list(group_a = simulate_breathtest_data(4, cov = cov,  k_mean = 0.02),
                group_b = simulate_breathtest_data(6, cov = cov,  k_mean = 0.015))
  } else if (data_subset == "rough_cross") {
    data = list(
      group_a = simulate_breathtest_data(4, noise = 1.5, student_t = 2., cov = cov,
                                         k_mean = 0.02),
      group_b = simulate_breathtest_data(6, noise = 1.5, student_t = 2., cov = cov,
                                         k_mean = 0.015))
  } else if (data_subset == "missing") {
    data = list(
      group_a = simulate_breathtest_data(4, noise = 2.5, student_t = 5, cov = cov,
                                         k_mean = 0.02, missing = 0.3 ),
      group_b = simulate_breathtest_data(6, noise = 2, student_t = 5, cov = cov,
                                         k_mean = 0.015, missing = 0.3))
  } else {
    return(NULL)
  }
  breathtestdata_to_editor_format(data, data_subset)
}

get_patient_data = function(data_source, data_subset, manual_select_data) {
  # Retrieves patient data
  if (is.null(data_source) | is.null(data_subset) | data_source == "" | data_subset == "" )
    return(NULL)

  #cat("get_patient_data: ", data_source, " ", data_subset, " ", manual_select_data, "\n")
  if (data_source == "usz_13c") {
      data = usz_13c_data(data_subset, manual_select_data)
  } else if (data_source == "usz_13c_a") {
      data = usz_13c_a_data(data_subset, manual_select_data)
  } else if (data_source == "usz_13c_d") {
      data = usz_13c_d_data(data_subset, manual_select_data)
  } else {
      return(NULL)
  }
  data$pdr = round(data$pdr,1)
  breathtestdata_to_editor_format(data)
}

# Data from usz_13c (Misselwitz data)
usz_13c_data = function(data_subset, manual_select_data){
  data("usz_13c", envir = environment())
  if (data_subset == "manual"){
    # When selection is empty, use first
    if (is.null(manual_select_data))
      manual_select_data = usz_13c$patient_id[1]
    data = usz_13c %>%
      filter(patient_id %in% manual_select_data)
  } else if (data_subset %in% c("no_header", "with_header")) {
    data = usz_13c  %>%
      filter(patient_id == "norm_001", group == "liquid_normal") %>%
      select(minute, pdr)
  } else if (data_subset == "two_patients") {
    data = usz_13c  %>%
      filter(patient_id %in% c("pat_001", "pat_002")) %>%
      select(patient_id, minute, pdr)
  } else if  (data_subset == "cross_over") {
    data = usz_13c  %>%
      filter(patient_id == "norm_007") %>%
      select(patient_id, group, minute, pdr)
  } else if  (data_subset == "cross_over_5") {
    data = usz_13c  %>%
      filter(patient_id <= "norm_005") %>%
      select(patient_id, group, minute, pdr)
  } else if (data_subset == "large_set") {
    set.seed(4711)
    use_id = sort(sample(unique(usz_13c$patient_id), 12))
    data = usz_13c %>%
      filter(patient_id %in% use_id) %>%
      select(patient_id, group, minute, pdr)
  } else if (data_subset == "very_large_set") {
    set.seed(41)
    use_id = sort(sample(unique(usz_13c$patient_id), 60))
    data = usz_13c %>%
      filter(patient_id %in% use_id) %>%
      select(patient_id, group, minute, pdr)
  }
  data
}


# Exotic data from usz_13c_a Kuyumcu et al
usz_13c_a_data = function(data_subset, manual_select_data){
  data("usz_13c_a", envir = environment())
  if (data_subset == "manual"){
    # When selection is empty, use first
    if (is.null(manual_select_data))
      manual_select_data = usz_13c_a$patient_id[1]
    data = usz_13c_a %>%
      filter(patient_id %in% manual_select_data)
  } else if (data_subset == "random_14") {
    set.seed(33)
    rs = sample(unique(usz_13c_a$patient_id), 14)
    data = usz_13c_a %>%
      filter(patient_id %in% rs)
  } else if (data_subset == "random_21") {
    set.seed(334)
    rs = sample(unique(usz_13c_a$patient_id), 21)
    data = usz_13c_a %>%
      filter(patient_id %in% rs)
  }
  data
}

# Data with known emptying time from MRI
usz_13c_d_data = function(data_subset, manual_select_data){
  data("usz_13c_d", envir = environment())
  if (data_subset == "manual"){
    # When selection is empty, use first
    if (is.null(manual_select_data))
      manual_select_data = usz_13c_d$patient_id[1]
    data = usz_13c_d %>%
      filter(patient_id %in% manual_select_data)
  } else if (data_subset == "subjects_2") {
    set.seed(33)
    rs = sample(unique(usz_13c_d$patient_id), 2)
    data = usz_13c_d %>%
      filter(patient_id %in% rs)
  } else if (data_subset == "all") {
    data = usz_13c_d
  }
  data
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
  comment = str_replace_all(comment,"^# *", "")
  comment(d) = comment
  d = cleanup_data(d)
  d
}

clear_search_text = function(id){
  span(
    helpText(HTML("Clear search box below to see<br> all coefficients)"),
             actionButton(paste0(id, "_button"), label = "Info", icon = icon("info"))),
    class = "help-block-right")
}

bt_datatable = function (cf){
  if (is.null(cf) || nrow(cf) ==0)
    return (NULL)
  search_options = list(paging = FALSE, searching = TRUE,
                        autoWidth = TRUE,
                        search = list(search = "t50"),
                        dom = 'Bfrtip',
                        buttons = c('copy', 'excel', 'pdf'))
  # Converting to factor is required for data table smart filtering
  cm = comment(cf)
  cf = cf %>%
    mutate_if(is.character, as.factor)
  DT::datatable(cf, rownames = FALSE, caption = cm,
                extensions  = 'Buttons',
                filter = "top",
                options = search_options)
}

# ------------------------------- Help text -----------------------------------------------

ace_options =
  "ace.edit('edit_data').setOptions({tabSize:16,showInvisibles:true,useSoftTabs:false});"

pop_content = c(
  data_only = "<code>No fit, data only</code> Select this before adding data or when changing data in the editor. With the others methods, any edit or selection triggers lengthy calculations.",
  nls = "<code>Individual curve fit (nls)</code> This is the method almost exclusively used in publications. It works for well-behaved breath test samples from healthy volunteers. For pathological records and when the recording time is too short compared to gastric emptying time, it can fail or give highly erratic results. Always use one of the population-based methods for clinical studies where multiple records are to be compared.",
  nlme = "<code>Mixed-model fit (nlme)</code>This method only works for multiple records that stabilize each other by borrowing strength. It can fail when many extreme records are included. Results are very stable even in presence of moderate outliers. If the algorithm converges, this is the recommended method for studies.",
  stan = "<code>Bayesian fit (Stan)</code>Most flexible method, but slow. It can be used with a single record, but really shows with multiple records from clinical studies. May need a few minutes to give accurate results; for a fast overview, select fewer iterations in the box below.",
  stan_group = "<code>Grouped Bayesian fit (Stan)</code>. This is an experimental variant, and in theory the most reliable one for studies when there are multiple groups with several records in each group. It uses a multi-level Bayesian model, but post-hoc tests shown in Details are not fully Bayesian. Use this with caution.",
  patient_test_data = "<code>Sample test data</code>, <br>13C records from University Hospital of Zürichz collection (usz_13c). Data from healthy volunteers (normals) are pairs from a cross-over study, for a liquid and a solid meal. <Easy> patient data can be fit by individual curves fits. <Difficult> patient data do not give valid results with individual fits, may converge using the nlme population fits, and do converge with Bayesian methods. ",
  no_header = "Single record with two columns for minute and pdr without header. This is the simplest data format accepted, but cannot be used for more than one record.",
  with_header = "Single records with two columns and headers <code>minute</code> and <code>pdr</code>. The results are the same as those from the two-column data set without headers.",
  two_patients = "With multiple records, the first column must have the header <code>patient_id</code>, followed by <code>minute</code> and <code>pdr</code>.",
  cross_over = "When there are multiple records for one patient such as in cross-over studies, four columns are required. The second column must be labeled <code>group</code>. Group labels must not contain spaces.",
  large_set = "Cross-over data from two normals, sampled with bags; and 10 data sets from patients. Data from 8 patients set were densely sampled with the BreathId device, those from patients `pat_044`` and `pat_094` were obtained with bags. Dense sampling is only visible when no fit or the individual curve fit (nls) is displayed. With the population-based nlme or Stan methods, subsampling to 5 (early) and 15 minute intervals is done to avoid convergence problems.",
  very_large_set = "Data from 7 normals, partially cross-over with different meals, and 73 patients. The Bayesian/Stan method needs about 3 minutes to fit these data, but gives stable estimates for all records.",
  edit_data = "Paste Excel data from your clipboard here after clearing current entries. Select items from the <b>Sample data</b> drop-down box to see supported formats:<ul><li>2 columns (minute, pdr) with and without header</li><li>3 columns (patient_id, minute, pdr) for one record per patient</li><li>4 columns (patient_id, group, minute, pdr) for patients and treatment groups</li><us><br><img src='excelsample.png'/><br><span style='font-size:12px'>Example of Excel data that can be pasted into the editor</span>",
  student_t_df = "With outliers in the data set, the fits are more robust when the residual data are modelled by the Student-t distribution than with normal (Gaussian) residuals. Computation time is somewhat longer with non-Gaussian options.",
  iter = "Number of iterations for Bayesian Stan sampling. More iteration give higher precision of the estimate. The default value of 200 is nice for a first look; use at least 500 iterations for a publishable result.",
  download_filtered = "Download the visible coefficients as CSV file which is readable from Excel. <br>If you only want some of the coefficients, use the filter boxes above the columns.
  <ul><li>To only return coefficient <code>m</code>, enter 'm' into the <code>parameter</code> search box. </li><li>Partial matching is allowed, e.g. <code>os</code> for <code>maes_ghoos</code> and <code>maes_goos_scint</code>.</li><li>Regular expressions are supported, most importantly a <code>$</code> for 'end-of-string'. For example, to suppress <code>maes_goos_scint</code>, use <code>maes_ghoos$</code>  or short <code>os$</code>.</li><li>To only return half-emptying times computed by the <code>maes_ghoos</code> method, enter <code>t50</code> in the parameter search box, and <code>os$</code> in the method box.</li></ul>"
)

## end popup


version_info =
  paste0("Packages breathtestcore (", packageVersion("breathtestcore"),
         "), breathteststan (", packageVersion("breathteststan"),
         "), breathtestshiny (", packageVersion("breathtestshiny"),
         "), rstan (", packageVersion("rstan"),
         "), StanHeaders (", packageVersion("StanHeaders"),
         ")")

about_text = paste(includeMarkdown("include/about.md"), version_info)



manual_subsets_a = function(){
  data("usz_13c_a", envir = environment())
  s = usz_13c_a %>%
    select(group, patient_id) %>%
    unique() %>%
    mutate(
      group = paste("Group",group)
    )
  split(s$patient_id, s$group)
}

manual_subsets_d = function(){
  data("usz_13c_d", envir = environment())
  as.list(unique(usz_13c_d$patient_id))
}

data_subsets = list(
  sim_data = list("Nice data, 1/subject" = "nice_1",
                  "Nice data, crossover" = "nice_cross",
                  "With outliers, crossover" = "rough_cross",
                  "Missing, outliers" = "missing"
  ),
  usz_13c = list("Manual" = "manual",
                 "One record, no header" = "no_header",
                 "One record with header" = "with_header",
                 "Records from 2 patients" = "two_patients",
                 "Crossover one" = "cross_over",
                 "Crossover 5" = "cross_over_5",
                 "Larger data set" = "large_set",
                 "Very large set" = "very_large_set"),
  usz_13c_d = list("2 subjects" = "subjects_2", "All records" = "all", "Manual" = "manual"),
  usz_13c_a = list("14 random" = "random_14", "21 random" = "random_21", "Manual" = "manual")
)

manual_subsets = list(
  sim_data = NULL,
  usz_13c =
    list(
      "Easy normals, solid and liquid" = c("norm_001", "norm_002", "norm_003"),
      "Easy patients" = c("pat_001", "pat_002", "pat_003"),
      "Difficult patients" = c("pat_051", "pat_016", "pat_033")
  ),
  usz_13c_d = manual_subsets_d(),
  usz_13c_a = manual_subsets_a()
)

