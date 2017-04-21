library(shinyjs)
library(shinyAce)
library(breathtestcore)
library(dplyr)

shinyServer(function(input, output, session) {

  clear_editor = function(){
    updateAceEditor(session, "edit_data", value = 1) # Funny method to clear
  }

  # Copy patient test data to editor
  observe({
    # Retrieve data
    td = input$patient_test_data
    if (is.null(td)) {
      clear_editor()
      return(NULL)
    }
    value = patient_test_data(td)
    updateAceEditor(session, "edit_data", value = value)
  })

  # Copy sample test data to editor
  observe({
    td = input$sample_data
    if (is.null(td) || td == "") return()
    value = sample_data(td)
    updateAceEditor(session, "edit_data", value = value )
  })

  # Clear editor when input button pressed
  observeEvent(input$clear_button, {
    clear_editor()
    updateSelectInput(session, "patient_test_data", selected = NA)
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
    switch(method,
      data_only = null_fit(data),
      nls = nls_fit(data),
      nlme = nlme_fit(data),
      stan = stan_fit(data,
                student_df = as.integer(input$student_df),
                iter = as.integer(input$iter))
    )
  })

  # --------- outputs -------------------------------------
  plot_height = function(){
    f = fit()
    length(unique(f$data$patient_id)) %/% ncol_facetwrap * 130L + 200L
  }

  output$fit_plot = renderPlot({
    f = fit()
    if (is.null(f)) return(NULL)
    plot(f) + ggplot2::facet_wrap(~patient_id, ncol = ncol_facetwrap)
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

# ------------- Help-related functions --------------------

  observeEvent(input$patient_test_data, {
    updateSelectInput(session, "sample_data", selected = NA)
  })

  observe({
    pop_control(session, input, "edit_data", "Data entry from clipboard")
  })

  observe({
    pop_select(session, input,  "sample_data", "Data formats")
  })

  observe({
    pop_select(session, input,  "method_a", "Fitting method")
  })

  # https://shiny.rstudio.com/articles/reconnecting.html
  session$allowReconnect(TRUE)

})
