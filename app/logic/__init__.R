options(digits = 3) # used in signif
# https://github.com/rstudio/shiny/issues/3626
options(shiny.useragg = TRUE)


box::use(
  utils[packageVersion, data],
  shiny[includeMarkdown],
  dplyr[...]
)

box::use(
  app/logic/datasets
)


#' @export
ncol_facetwrap = 4 # for facet_wrap, number of columns

#' @export
chains = min(parallel::detectCores(logical = TRUE), 2)

# ------------------------------- Help text ---------------------------------------------
#' @export
pop_content = c(
  append = "When unchecked, only the uploaded file or the uploaded files are displayed and
fitted; the editor is automatically cleared before each upload. When checked, the editor
is not cleared, data are appended to the exisiting ones. Note the slightly different
behaviour of the grouping variable when no group name is given. ",
  upload = "You can select or drag/drop multiple files of the same or mixed format at a time. Use the checkbox below if you prefer to add one file after the other.<br><b>When drag/drop does not respond (e.g. on Firefox), use another browser (e.g. Chrome) or the Browse button to select files.",
  data_only = "<code>No fit, data only</code> Select this before adding data or when changing data in the editor. With the others methods, any edit or selection triggers lengthy calculations.",
  nls = "<code>Individual curve fit (nls)</code> This is the method almost exclusively used in publications. It works for well-behaved breath test samples from healthy volunteers. For pathological records and when the recording time is too short compared to gastric emptying time, it can fail or give highly erratic results. Always use one of the population-based methods for clinical studies where multiple records are to be compared.",
  nlme = "<code>Mixed-model fit (nlme)</code>This method only works for multiple records that stabilize each other by borrowing strength. It can fail when many extreme records are included. Results are very stable even in presence of moderate outliers. If the algorithm converges, this is the recommended method for studies.",
  stan = "<code>Bayesian fit (Stan)</code>Most flexible method, but slow. It can be used with a single record, but really shows with multiple records from clinical studies. May need a few minutes to give accurate results; for a fast overview, select fewer iterations in the box below.",
  stan_group = "<code>Grouped Bayesian fit (Stan)</code>. This is an experimental variant, and in theory the most reliable one for studies when there are multiple groups with several records in each group. It uses a multi-level Bayesian model, but post-hoc tests shown in Details are not fully Bayesian. Use this with caution.",
  usz_13c = "<code>Sample test data</code>, <br>13C records from University Hospital of Zürich collection (usz_13c). Data from healthy volunteers (normals) are pairs from a cross-over study, for a liquid and a solid meal. <i>Easy</i> patient data can be modelled by individual curves fits. <i>Difficult</i> patient data do not give valid results with individual fits, may converge using the nlme population fits, and always converge with Bayesian methods. ",
  no_header = "Single record with two columns for minute and pdr without header. This is the simplest data format accepted, but cannot be used for more than one record.",
  with_header = "Single records with two columns and headers <code>minute</code> and <code>pdr</code>. The results are the same as those from the two-column data set without headers.",
  two_patients = "With multiple records, the first column must have the header <code>patient_id</code>, followed by <code>minute</code> and <code>pdr</code>.",
  cross_over = "When there are multiple records for one patient such as in cross-over studies, four columns are required. The second column must be labeled <code>group</code>. Group labels must not contain spaces.",
  large_set = "Cross-over data from two normals, sampled with bags; and 10 data sets from patients. Data from 8 patients set were densely sampled with the BreathId device, those from patients `pat_044`` and `pat_094` were obtained with bags. Dense sampling is only visible when no fit or the individual curve fit (nls) is displayed. With the population-based nlme or Stan methods, subsampling to 5 (early) and 15 minute intervals is done to avoid convergence problems.",
  very_large_set = "Data from 7 normals, partially cross-over with different meals, and 73 patients. The Bayesian/Stan method needs about 3 minutes to fit these data, but gives stable estimates for all records.",
  edit_data = "Paste Excel data from your clipboard here after clearing current entries. As examples, select items from the <b>Sample data</b> drop-down box to see supported formats:<ul><li>2 columns (minute, pdr) with and without header</li><li>3 columns (patient_id, minute, pdr) for one record per patient</li><li>4 columns (patient_id, group, minute, pdr) for patients and treatment groups</li></ul>Data read in:<pre>",
  student_t_df = "With outliers in the data set, the fits are more robust when the residual data are modelled by the Student-t distribution than with normal (Gaussian) residuals. Computation time is somewhat longer with non-Gaussian options.",
  iter = "Number of iterations for Bayesian Stan sampling. More iteration give higher precision of the estimate. The default value of 200 is nice for a first look; use at least 500 iterations for a publishable result.",
  download_filtered = "Download the visible coefficients as CSV file which is readable from Excel. <br>If you only want some of the coefficients, use the filter boxes above the columns.
  <ul><li>To only return coefficient <code>m</code>, enter 'm' into the <code>parameter</code> search box. </li><li>Partial matching is allowed, e.g. <code>os</code> for <code>maes_ghoos</code> and <code>maes_goos_scint</code>.</li><li>Regular expressions are supported, most importantly a <code>$</code> for 'end-of-string'. For example, to suppress <code>maes_goos_scint</code>, use <code>maes_ghoos$</code>  or short <code>os$</code>.</li><li>To only return half-emptying times computed by the <code>maes_ghoos</code> method, enter <code>t50</code> in the parameter search box, and <code>os$</code> in the method box.</li></ul>",
  sim_data = "Simulated data created by function <br><code>simulate_breathtest_data/breathtestcore</code>; the base curve is an exponential beta function as used by Maes/Ghoos. Details see in data set dropdown below.",
  nice_1 =  "One group generated with <br><code>simulate_breathtest_data(7,  k_mean = 0.015)</code>",
  nice_cross = "Two groups simulated by <br><code>simulate_breathtest_data(4, k_mean = 0.02)</code><br><code>simulate_breathtest_data(6, k_mean = 0.015)</code>",
  rough_cross = "Two groups with noise and strong outliers generated by<br><code>simulate_breathtest_data(4, noise = 1.5, student_t_df = 2, k_mean = 0.02)</code><br><code>simulate_breathtest_data(6, noise = 1.5, student_t_df = 2., k_mean = 0.015)</code>.",
  missing = "Two groups with moderate outliers and 30% missing data generated by <br><code>simulate_breathtest_data(4, noise = 2.5, student_t_df = 5, k_mean = 0.02, missing = 0.3)</code> <br><code>simulate_breathtest_data(6, noise = 2, student_t_df = 5, k_mean = 0.015, missing = 0.3)</code>",
  usz_13c_d = "Subset of <code>usz_13c</code> data from USZ data (Kuyumcu et al., Gastric secretion...). For this data set, gastric emptying times from MRI measurements are attached and can be used to estimate the reliability of the 13C method.",
  cross_over_5 = "Five records with two meals; this example has been chosen because is does not converge with the mixed-model fit, which indicates that single curve fit might also be unreliable.",
  manual = "Select individual records from the dropdown below",
  subjects_2 = "Two subjects with 3 and 2 meals in cross-over",
  all = "All records from USZ study with 3 meals, some are missing",
  usz_13c_a = "13C time series PDR data from three different groups in a randomized (= not-crossover) design from Gastroenterology and Hepatology, University Hospital Zurich. These time series present a challenge for algorithms. Try the Bayesian mode, but do not trust such data nevertheless.",
  random_14 = "14 random records from <code>usz_13c_a</code>",
  random_21 = "21 random records from <code>usz_13c_a</code>"
)

## end popup


#' @export
version_info =
  paste0("Packages breathtestcore (", packageVersion("breathtestcore"),
         "), breathteststan (", packageVersion("breathteststan"),
         "), rstan (", packageVersion("rstan"),
         "), StanHeaders (", packageVersion("StanHeaders"),
         "), Shiny (", packageVersion("shiny"),
         ")")

app_root = rprojroot::find_root(rprojroot::has_file("app.R"))
#' @export
about_text = paste(includeMarkdown(
  file.path(app_root, "app/include/about.md")), version_info)

#' @export
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

#' @export
manual_subsets = list(
  sim_data = NULL,
  usz_13c =
    list(
      "Easy normals, solid and liquid" = c("norm_001", "norm_002", "norm_003"),
      "Easy patients" = c("pat_001", "pat_002", "pat_003"),
      "Difficult patients" = c("pat_051", "pat_016", "pat_033")
    ),
  usz_13c_d = datasets$manual_subsets_d(),
  usz_13c_a = datasets$manual_subsets_a()
)
