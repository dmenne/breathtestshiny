library(shinyjs)
library(shinyAce)
library(breathtestcore)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

shinyServer(function(input, output, session) {
  btns = list("Details", "Summary", "Group differences")

  info_observers = sapply(btns, function(btn_title) {
    btn = gsub(" ", "_", tolower(btn_title))
    pnl = paste0(btn, "_button")
    txt = includeMarkdown(paste0("include/", btn, ".md"))
    dlg = modalDialog(
      txt,
      title = btn_title,
      footer = modalButton("Close"),
      size = "m",
      easyClose = TRUE
    )
    observeEvent(input[[pnl]], showModal(dlg))
  })

  clear_editor = function() {
    updateAceEditor(session, "edit_data", value = 1) # Funny method to clear
  }

  # Copy patient test data to editor
  observe({
    clear_editor()
    # Retrieve data
    data_source = isolate(input$data_source)
    data_subset = input$data_subset
    manual_select_data = input$manual_select_data
    #cat(data_source, "-", data_subset, "-", manual_select_data, "\n")

    if (is.null(data_subset) | is.null(data_source)) {
      clear_editor()
      return(NULL)
    }
    # Clear manual selection if it is not manual mode
    if (data_subset != "manual") {
      updateSelectInput(session, "manual_select_data", selected = NA)
    }
    if (data_source == "sim_data") {
      value = get_simulated_data(data_subset)
    } else {
      value = get_patient_data(data_source, data_subset, manual_select_data)
    }
    updateAceEditor(session, "edit_data", value = value)
  })

  # Clear editor when input button pressed
  observeEvent(input$clear_button, {
    clear_editor()
    updateSelectInput(session, "manual_select_data", selected = NA)
  })


  # Download image on button press
  # https://shiny.rstudio.com/articles/generating-reports.html

  output$download_image_button = downloadHandler(
    filename = function()
      paste0('breathtest_', get_data()$patient_id[1], "_", Sys.Date(),'.png'),
    content = function(file){
      f = fit()
      if (is.null(f)) return(NULL)
      n_patient = length(unique(get_data()$patient_id))
      if ((n_patient %% ncol_facetwrap) == 1)
        ncol_facetwrap = ncol_facetwrap - 1
      width = min(n_patient, ncol_facetwrap)*3 + 1.5 # Make this variable
      p = plot(f) +
        facet_wrap(~patient_id, ncol = ncol_facetwrap) +
        theme(legend.key.size = unit(2,"line")) +
        guides(colour = guide_legend(override.aes = list(size = 2)))
      ggsave(file, plot = p, device = "png", width = width,
             height = plot_height()/50)
    }
  )

  # Retrieve data from editor
  get_data = reactive({
    data = input$edit_data
    d = format_data(data)
    if (is.null(d))
      return(NULL)
    validate(
      need(
        (input$fit_method == "stan") || (nrow(d) >= 5),
        "At least 5 data values required. Stan fit might work."
      ),
      need(
        (input$fit_method != "nlme") ||
        (length(unique(paste(d$patient_id, d$group, sep = "_"))) > 1L),
        "At least 2 records required. Try individual curve fit or Bayesian fit instead."
      ),
      need(
        (input$fit_method != "stan_group" ) || (length(unique(d$group)) > 1L),
        "Multiple groups required. Try individual curve fit or Bayesian fit instead."
      )
    ) # end validate
    d
  })

  # Compute fit when button pressed or method changed
  fit = reactive({
    method = input$fit_method
    data = get_data()
    if (is.null(data))
      return(NULL)
    #save(data, file= "ndata.rda")
    switch(
      method,
      data_only = null_fit(data),
      nls = nls_fit(data),
      nlme = nlme_fit(data),
      stan = stan_fit( # in package breathteststan
        data,
        chains = 2,
        student_t_df = as.integer(input$student_t_df),
        iter = as.integer(input$iter)
      ),
      stan_group = stan_group_fit( # in package breathteststan
        data,
        chains = 2,
        student_t_df = as.integer(input$student_t_df),
        iter = as.integer(input$iter)
      )
    )
  })

  # Returns coefficients of fit and comment
  coef_fit = function() {
    f  = fit()
    if (is.null(f))
      return(NULL)
    cf = coef(f)
    if (is.null(cf))
      return(NULL)
    cf$value = signif(cf$value, as.integer(options("digits")))
    comment(cf) = comment(f$data)
    cf
  }

  # --------- outputs -------------------------------------
  output$coef_table = DT::renderDataTable({
    cf = coef_fit()
    bt_datatable(cf)
  })

  output$coef_by_group_table = DT::renderDataTable({
    f = fit()
    if (inherits(f, "breathtestnullfit"))
      return(NULL)
    cf =  try(coef_by_group(f), silent = TRUE )
    validate(
      need(
        !is(cf, "try-error"),
        "To estimate means, you need multiple data sets for some of the groups."
      )
    )
    bt_datatable(cf)
  })

  output$coef_by_group_diff_table = DT::renderDataTable({
    f = fit()
    if (inherits(f, "breathtestnullfit"))
      return(NULL)
    cf =  try(coef_diff_by_group(fit()), silent = TRUE)
    validate(
      need(
        !is(cf, "try-error"),
        "To estimate group differences, you need multiple data sets for some of the groups."
      )
    )
    bt_datatable(cf)
  })


  plot_height = function() {
    n_patient = length(unique(get_data()$patient_id))
    n_patient %/% ncol_facetwrap * 130L + 200L
  }

  output$fit_plot = renderPlot({
    f = fit()
    if (is.null(f))
      return(NULL)
    plot(f) +
      facet_wrap(~patient_id, ncol = ncol_facetwrap) +
      theme(aspect.ratio = 0.8)
  }, height = plot_height)


  # Panel logic --------------------
  observe({
    data_source = input$data_source
    data_subset = isolate(input$data_subset)
    if (data_subset != "") {
      updateSelectInput(session, "data_subset",
                        choices = data_subsets[[data_source]])
    } else {
      updateSelectInput(session, "data_subset",
                        choices = data_subsets[[data_source]],
                        selected = "cross_over_5")
    }
  })

  observe({
    data_subset = input$data_subset
    data_source = input$data_source
    if (data_subset == "manual")
      updateSelectInput(session, "manual_select_data",
                        choices = manual_subsets[[data_source]])
  })


  # ------------- Hide panel logic --------------------
  observe({
    has_fit = input$fit_method != "data_only"
    cf = coef_fit()
    if (is.null(cf)) return(NULL)
    has_groups = ifelse(!has_fit, FALSE,
                        length(unique(cf$group)) > 1)
    toggle(
      condition = has_fit,
      selector = list(
        "#main_panel li a[data-value=details_panel]",
        "#main_panel li a[data-value=summary_panel]"
      )
    )
    toggle(condition = has_groups,
           selector = "#main_panel li a[data-value=group_differences_panel]")
  })

  # ------------- Help-related functions --------------------

  # Clear sample data selection when patient data are changed
  observeEvent(input$manual_select_data, {
    updateSelectInput(session, "simulated_data", selected = NA)
  })

  observe({
    toggle("download_filtered", condition = !is.null(coef_fit()))
  })

  observe({
    pop_control(session, input, "edit_data", "Data entry from clipboard")
  })

  observe({
    pop_control(session,
                input,
                "download_filtered",
                "Download coefficients as CSV-file")
  })

  observe({
    input$fit_method
    pop_control(session, input,  "student_t_df", "Outlier handling")
  })

  observe({
    input$fit_method
    pop_control(session, input,  "iter", "Number of iterations Stan sampling")
  })

  # Upload
  observe({
    pop_control(session, input,  "upload", "Upload breathtest data")
  })

  # Append data
  observe({
    pop_control(session, input,  "append", "Append data in editor")
  })

  # Select boxes with per-item description
  observe({
    pop_select(session, input,  "fit_method", "Fitting method")
  })

  observe({
    input$fit_method
    pop_select(session, input,  "data_subset", "Sample data")
  })

  output$about = renderText({
    about_text
  })

  # --------------- Uploading files -----------------------------------------
  dt_list = reactive({
    inFile <- input$upload # When upload changes
    if (is.null(inFile)) return(NULL)
    inFile$status = NA
    dt_list = list()
    n_files = nrow(inFile)
    if (n_files == 0) return(NULL)
    for (i in 1:n_files) {
      # Restore original filename for better messaging
      src_file = inFile[i,"datapath"]
      dest_file = file.path(dirname(src_file),inFile[i,"name"])
      suppressWarnings(file.remove(dest_file)) # In case it exists
      file.rename(src_file, dest_file)
      # Read file
      dt = try(read_any_breathtest(dest_file), silent = TRUE)
      if (inherits(dt, "try-error")) {
        inFile[i,"status"] = str_replace(dt, dirname(src_file), "")
      } else {
        inFile[i, "status"] = "Ok"
        dt_list = c(dt_list, dt)
      }
    }
    if (length(dt_list) == 0)  return(NULL)
    attr(dt_list, "n_files") = n_files
    dt_list
  })

  observe({
    dt_s = dt_list()
    if (is.null(dt_s)) return(NULL)
    selected_records = NULL
    n_files = attr(dt_s, "n_files")
    if (!is.null(input$ok_patient)) {
      selected_records  = isolate(input$select_records)
      # clear for next run
      updateCheckboxGroupInput(session, "select_records", selected = character(0) )
      if (!is.null(selected_records))
        dt_s = dt_s[as.integer(selected_records)]
    }
    # Assume there are data
    # When there is only one file, and it contains several records,
    # let user select. Can happen with xml files from breathid
    if (is.null(selected_records) && n_files == 1 && length(dt_s) > 1) {
      showModal(patient_modal(dt_s))
    } else {
      dt = breathtestdata_to_editor_format(dt_s) # will do cleanup_data
      # Append if required
      if (isolate(input$append)) {
        dt_old = unlist(isolate(input$edit_data))
        # Remove header
        dt = str_replace(dt,".*?\\n", "")
        dt = str_c(dt_old, "\n", dt)
      }
      updateAceEditor(session, "edit_data", value = dt)
    }
  })

  patient_modal = function(dt_s, failed = FALSE){
    pt = seq_along(dt_s)
    names(pt) = paste("Patient", purrr::map_chr(dt_s, "patient_id"),
               purrr::map_chr(dt_s, "record_date"),
               purrr::map_chr(dt_s, "start_time"))
    modalDialog(
      span(
        "The record contains data from several patients runs."),
      checkboxGroupInput("select_records", "Select at least one",
                         pt),
      if (failed)
        div(tags$b("You must select at least one record", style = "color: red;")),
      footer = tagList(
        actionButton("ok_patient", "Ok")
      ),
      size = "s"
    )
  }

  observeEvent(input$ok_patient, {
    selected_records  = as.integer(isolate(input$select_records))
    # Must select
    if (length(selected_records) == 0)
      showModal(patient_modal(isolate(dt_list()), TRUE))
    else {
      removeModal()
    }
  })

  # Required for RInno
  session$onSessionEnded(function() {
    stopApp()
    if (Sys.getenv("RSTUDIO") != "1")
     q("no")
  })

  # https://shiny.rstudio.com/articles/reconnecting.html
  session$allowReconnect(TRUE)

})
