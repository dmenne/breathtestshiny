box::use(
  dplyr[...],
  utils[data],
  stringr[str_c],
  breathtestcore[simulate_breathtest_data]
)

box::use(
  app/logic/breathtestdata_to_editor_format[breathtestdata_to_editor_format]
)

#' @export

# Exotic data from usz_13c_a Kuyumcu et al
usz_13c_a_data = function(data_subset, manual_select_data) {
  data("usz_13c_a", package = "breathtestcore", envir = environment())
  if (data_subset == "manual") {
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

#' @export
# Data with known emptying time from MRI
usz_13c_d_data = function(data_subset, manual_select_data) {
  data("usz_13c_d", package = "breathtestcore", envir = environment())
  if (data_subset == "manual") {
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

#' @export
# Data from usz_13c (Misselwitz data)
usz_13c_data = function(data_subset, manual_select_data) {
  data("usz_13c", package = "breathtestcore", envir = environment())
  if (data_subset == "manual") {
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

#' @export
manual_subsets_a = function() {
  box::use(utils[data])
  data("usz_13c_a", package = "breathtestcore", envir = environment())
  s = usz_13c_a %>%
    select(group, patient_id) %>%
    unique()
  # Strange error message when using mutate in the following
  # error in cnd(.subclass, ..., message = message) :
  # VECTOR_ELT() can only be applied to a 'list', not a 'language'
  s$group = str_c("Group_", s$group)
  split(s$patient_id, s$group)
}

#' @export
manual_subsets_d = function() {
  box::use(utils[data])
  data("usz_13c_d", package = "breathtestcore", envir = environment())
  as.list(unique(usz_13c_d$patient_id))
}

#' @export
get_patient_data = function(data_source, data_subset, manual_select_data) {
  # Retrieves patient data
  if (is.null(data_source) || is.null(data_subset) ||
      data_source == "" || data_subset == "")
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
  data$pdr = round(data$pdr, 1)
  breathtestdata_to_editor_format(data)
}


#' @export
get_simulated_data = function(data_subset) {
  # Retrieves simulated data
  if (is.null(data_subset) || data_subset == "") return(NULL)
  cov = # empirical covariance
    structure(c(188, -0.026, -2.04, -0.026, 7.74e-06, 0.000477, -2.04, 0.000477, 0.182),
              .Dim = c(3L, 3L), .Dimnames = list(c("m", "k", "beta"), c("m", "k", "beta")))
  if (data_subset == "nice_1") {
    data = simulate_breathtest_data(7,  cov = cov, k_mean = 0.015)
  } else if (data_subset == "nice_cross") {
    data = list(group_a = simulate_breathtest_data(4, cov = cov,  k_mean = 0.02),
                group_b = simulate_breathtest_data(6, cov = cov,  k_mean = 0.015))
  } else if (data_subset == "rough_cross") {
    data = list(
      group_a = simulate_breathtest_data(4, noise = 1.5, student_t_df = 2., cov = cov,
                                         k_mean = 0.02),
      group_b = simulate_breathtest_data(6, noise = 1.5, student_t_df = 2., cov = cov,
                                         k_mean = 0.015))
  } else if (data_subset == "missing") {
    data = list(
      group_a = simulate_breathtest_data(4, noise = 2.5, student_t_df = 5, cov = cov,
                                         k_mean = 0.02, missing = 0.3),
      group_b = simulate_breathtest_data(6, noise = 2, student_t_df = 5, cov = cov,
                                         k_mean = 0.015, missing = 0.3))
  } else {
    return(NULL)
  }
  breathtestdata_to_editor_format(data, data_subset)
}
