library(shinyjs)
library(shinyAce)
library(breathtestcore)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

shinyServer(function(input, output, session) {

  btns = list("Details", "Summary", "Group differences")

  info_observers = sapply(btns, function(btn_title){
      btn = gsub(" ", "_", tolower(btn_title))
      pnl = paste0(btn,"_button")
      txt = includeMarkdown(paste0("include/", btn,".md"))
      dlg = modalDialog(txt, title = btn_title,
                        footer = modalButton("Close"),
                        size = "m", easyClose = TRUE)
      observeEvent(input[[pnl]], showModal(dlg))
  })

  clear_editor = function(){
    updateAceEditor(session, "edit_data", value = 1) # Funny method to clear
  }

  # Copy patient test data to editor
  observe({
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
    if (data_source == "sim_data"){
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

  get_data = reactive({
    data = input$edit_data
    d = format_data(data)
    if (is.null(d)) return(NULL)
    validate(
      need(input$method_a == "stan" || nrow(d) >= 10,
           "At least 10 data values required."),
      need(input$method_a != "nlme" ||
             length(unique(paste(d$patient_id, d$group, sep = "_"))) >= 2,
           "At least 2 records required. Try single-curve or Bayesian fit instead.")
    )
    d
  })

  # Compute fit when button pressed or method changed
  fit = reactive({
    method = input$method_a
    data = get_data()
    if (is.null(data))
      return(NULL)
    #save(data, file= "ndata.rda")
    switch(method,
      data_only = null_fit(data),
      nls = nls_fit(data),
      nlme = nlme_fit(data),
      stan = stan_fit(data, chains = 1,
                student_t_df = as.integer(input$student_t_df),
                iter = as.integer(input$iter))
    )
  })

  # Returns coefficients of fit and comment
  coef_fit = function(){
    f  = fit()
    if (is.null(f)) return(NULL)
    cf = coef(f)
    if (is.null(cf)) return(NULL)
    cf$value = signif(cf$value, as.integer(options("digits")))
    comment(cf) = comment(f$data)
    cf
  }

  # --------- outputs -------------------------------------
  output$coef_table = DT::renderDataTable({
    cf =  coef_fit()
    bt_datatable(cf)
  })

  output$coef_by_group_table = DT::renderDataTable({
    f = fit()
    if (inherits(f, "breathtestnullfit"))
      return(NULL)
    cf =  coef_by_group(f)
    bt_datatable(cf)
  })

  output$coef_by_group_diff_table = DT::renderDataTable({
    f = fit()
    if (inherits(f, "breathtestnullfit"))
      return(NULL)
    cf =  coef_diff_by_group(fit())
    bt_datatable(cf)
  })


  plot_height = function(){
    n_patient = length(unique(get_data()$patient_id))
    n_patient %/% ncol_facetwrap * 130L + 200L
  }

  output$fit_plot = renderPlot({
    f = fit()
    if (is.null(f)) return(NULL)
    #capture.output(str(f), file = stderr())
    plot(f) +
      facet_wrap(~patient_id, ncol = ncol_facetwrap) +
      theme(aspect.ratio = 1)
  }, height = plot_height)


# --------------- Workspace-related functions -------------------------------
  data_dir = function(){
    u = uid()
    ifelse(is.null(u), NULL, file.path(data_root, u))
  }

  output$use_link = reactive({
    ifelse(is.null(uid()), "", "Use this link to recover data")
  })

  # Mark that named user workspace is valid
  observe({
    shinyjs::toggle("create_workspace", condition = is.null(uid()))
    shinyjs::toggle("userid", condition = !is.null(uid()))
  })

  # Display current workspace name
  observe({
    updateActionButton(session, "userid", url() )
  })

  # Create id for workspace
  observeEvent(input$create_workspace, {
    new_uid = paste0("?uid=", digest::digest(rnorm(1), "xxhash32" ))
    session$sendCustomMessage(type = 'replace_url',
                              message = new_uid )
  })

  # When url is passed in browser, make it a valid id by replacing
  # non-alphanumeric characters
  uid = reactive({
    u = parseQueryString(session$clientData$url_search)[["uid"]]
    # Make it a valid subdirectory name
    cleanup_uid(u)
  })


  # Watch for url name to identify workspace
  url = reactive({
    cd = session$clientData
    url1 = cd$url_hostname
    if (!is.null(cd$url_port))
      url1 = paste0(url1, ":", cd$url_port)
    ifelse(is.null(uid()), url1, paste0(url1, "/?uid=", uid()))
  })

  # ------------- Panel logic --------------------
  observe({
    data_source = input$data_source
    if (!is.null(data_source))
      updateSelectInput(session, "data_subset",
                        choices = data_subsets[[data_source]] )
  })

  observe({
    data_subset = input$data_subset
    data_source = input$data_source
    if (data_subset == "manual")
      updateSelectInput(session, "manual_select_data",
                        choices = manual_subsets[[data_source]] )
  })


  # ------------- Hide panel logic --------------------
  observe({
    has_fit = input$method_a != "data_only"
    has_groups = ifelse(!has_fit, FALSE, length(unique(coef_fit()$group)) >1 )
    toggle(condition = has_fit,
      selector = list(
        "#main_panel li a[data-value=details_panel]",
        "#main_panel li a[data-value=summary_panel]"))
    toggle(condition = has_groups,
           selector = "#main_panel li a[data-value=group_differences_panel]")
  })

  # ------------- Help-related functions --------------------

  # Clear sample data selection when patient data are changed
  observeEvent(input$manual_select_data, {
    updateSelectInput(session, "simulated_data", selected = NA)
  })

  observe({
    toggle("download_filtered", condition = !is.null(coef_fit()) )
  })

  observe({
    pop_control(session, input, "edit_data", "Data entry from clipboard")
  })

  observe({
    pop_control(session, input, "download_filtered", "Download coefficients as CSV-file")
  })

  observe({
    input$method_a
    pop_control(session, input,  "student_t_df", "Outlier handling")
  })

  observe({
    input$method_a
    pop_control(session, input,  "iter", "Number of iterations Stan sampling")
  })

# Select boxes with per-item description
  observe({
    pop_select(session, input,  "method_a", "Fitting method")
  })

  observe({
    input$method_a
    pop_select(session, input,  "data_subset", "Sample data")
  })

  output$about = renderText({
    about_text
  })


  # https://shiny.rstudio.com/articles/reconnecting.html
  session$allowReconnect(TRUE)

})
