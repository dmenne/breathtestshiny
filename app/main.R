# library functions
box::use(
  shiny[...],
  shinyjs[useShinyjs, js, extendShinyjs, toggle],
  shinycssloaders[withSpinner],
  shinyBS[popify, bsPopover, bsModal],
  shinyAce[aceEditor, updateAceEditor],
  breathtestcore[null_fit, nls_fit, nlme_fit, read_any_breathtest,
                 coef_by_group, coef_diff_by_group],
  breathteststan[stan_fit],
  dplyr[...],
  ggplot2[facet_wrap, theme, guides],
  stats[coef],
  stringr[str_c, str_replace],
  utils[str],
  glue[glue],
  methods[is]
)

# constants
box::use(
  app/logic[about_text, pop_content, data_subsets,
            manual_subsets, ncol_facetwrap, chains],
  app/logic/breathtestdata_to_editor_format[breathtestdata_to_editor_format],
  app/logic/bt_datatable[bt_datatable]

)

# https://github.com/Appsilon/rhino/discussions/279
shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))


# local functions
box::use(
  app/logic/clear_search_text[clear_search_text],
  app/logic/format_data[...],
  app/logic/popup[...],
  app/logic/datasets[get_patient_data, get_simulated_data],

)

# TODO: hello

#' @export
ui = function(id) {
  ns = NS(id)
  shinyUI(
  fluidPage(
    useShinyjs(),
    extendShinyjs(
      text = "shinyjs = { ...shinyjs, ...App.extensions }",
      functions = c("clearUpload")),
    titlePanel("Gastric emptying from 13C Breath Test Data"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("fit_method"),
          "Fit Method",
          choices =
            c(
              "No fit, data only" = "data_only",
              "Individual curve fit (nls)" = "nls",
              "Mixed-model fit (nlme) " = "nlme",
              "Bayesian fit (Stan)" = "stan"
              #,"Grouped Bayesian fit" = "stan_group"
            ),
          selected = "nls"
        ),
        conditionalPanel(
          "input.fit_method == 'stan' || input.fit_method == 'stan_group'",
          ns = ns,
          selectInput(
            ns("iter"),
            "Iterations",
            choices = c(200, 500, 1000, 2000),
            selected = 200
          )
        ),
        tabsetPanel(
          tabPanel(
            title = "Uploads",
            br(),
            popify(
              fileInput(
                ns("upload"),
                "Select or drag/drop file(s)",
                multiple = TRUE,
                accept = c("text/plain", "text/csv", "text/xml",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                  "application/vnd.ms-excel"),
                buttonLabel = "Browse file",
                placeholder = "Drag file here"
              ),
              "Upload 13C files",
              pop_content["upload"],
              "right"
            ),
            checkboxInput(ns("append"), "Append selected data", value = FALSE),
            value = "uploads_panel"
          ),
          # End uploads_panel

          tabPanel(
            title = "Demo",
            conditionalPanel(
              "input.fit_method == 'stan' || input.fit_method == 'stan_group'",
              ns = ns,
              selectInput(
                ns("student_t_df"),
                "Expected outliers",
                choices = c(
                  "None - Gaussian" = 10,
                  "Few - Student-t 5 df" = 5,
                  "Strong - Student-t 3 df" = 3
                )
              )
            ),
            # end conditionalPanel
            selectInput(
              ns("data_source"),
              "Data source",
              choices = c(
                "Simulated data " = "sim_data",
                "Partial crossover (usz_13c)" = "usz_13c",
                "Crossover (usz_13c_d)" = "usz_13c_d",
                "Exotic (usz_13c_a)" = "usz_13c_a"
              ),
              selected = "usz_13c"
            ),
            selectInput(
              ns("data_subset"),
              "Data subset",
              choices = data_subsets["usz_13c"],
              selected = "cross_over_5"
            ),
            conditionalPanel(
              "input.data_subset == 'manual'",
              ns = ns,
              selectInput(
                ns("manual_select_data"),
                "Select data",
                choices = NULL,
                multiple = TRUE
              )
            ),
            value = "demo_panel"
          ), # end demo_panel
          id = ns("samples_panel"),
          selected = "demo_panel"
        ),
        # End tabsetpanel
        hr(),
        HTML('<a href="https://dmenne.github.io/breathtestcore/articles/data_formats.html#vendor-specific-formats">Supported Formats</a>'),
        # The following should not be moved to the server
        bsPopover(ns("show_pop"),  "Enable/disable all popups", "", "right"),
        checkboxInput(ns("show_pop"), "Show popover help", value = TRUE),
        actionLink(ns("about"), "About"),
        width = 3
      ),
      # end sidebarPanel
      mainPanel(
        tabsetPanel(
          id = ns("main_panel"),
          tabPanel(
            title = "Data",
            aceEditor(ns("edit_data"), "", mode = "plain_text", tabSize = 16,
                      useSoftTabs = FALSE, showInvisibles = FALSE,
                      setBehavioursEnabled = FALSE,
                      placeholder = "Paste columns from Excel here"),
            actionButton(ns("clear_button"), "Clear", icon = icon("eraser")),
            downloadButton(ns("download_image_button"), "Image"),
            withSpinner(plotOutput(ns("fit_plot"), height = "auto")),
            hr(),
            value = ns("data_panel")
          ),
          # Data tabPanels
          tabPanel(
            title = "Details",
            value = ns("details_panel"),
            clear_search_text(ns("details")),
            h2("Per Patient/Record results"),
            withSpinner(DT::dataTableOutput(ns("coef_table")))
          ),
          tabPanel(
            title = "Summary",
            value = ns("summary_panel"),
            clear_search_text("summary"),
            h2("Per Group Means"),
            withSpinner(DT::dataTableOutput(ns("coef_by_group_table")))
          ),
          # End Summary tabPanel
          tabPanel(
            title = "Group differences",
            value = ns("group_differences_panel"),
            clear_search_text("group_differences"),
            h2("Difference means between groups"),
            withSpinner(DT::dataTableOutput(ns("coef_by_group_diff_table")))
          ) # End Group differences tabPanel
        ), # end tabsetPanel
        bsModal(
          id = ns("about_tab"),
          title = "About breathtestshiny",
          trigger = ns("about"),
          size = "large",
          HTML(about_text)
        )
      ) # end mainPanel
    ) # end sidebarLayout
  ) # fluidpage
) # shinyUI
} # module function

#' @export
server = function(id) {
  moduleServer(id, function(input, output, session) {
  ns = session$ns
  lapply(list("Details"), function(btn_title) {
    btn = gsub(" ", "_", tolower(btn_title))
    pnl = paste0(btn, "_button")
    app_root = rprojroot::find_root(rprojroot::has_file("app.R"))
    txt = includeMarkdown(paste0(app_root, "/app/include/", btn, ".md"))
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
    updateAceEditor(session, "edit_data", value = "")
    clear_upload()
  }

  clear_upload = function() {
    js$clearUpload(id)
  }

  observeEvent(input$about, {
    #shinyBS::toggleModal(session, ns("about"))
  })

  # Copy patient test data to editor
  observe({
    clear_editor()
    # Retrieve data
    data_source = isolate(input$data_source)
    data_subset = input$data_subset
    manual_select_data = input$manual_select_data
    #cat(data_source, "-", data_subset, "-", manual_select_data, "\n")

    if (is.null(data_subset) || is.null(data_source)) {
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
    pop_control(session, input,  "edit_data", "Data entry from clipboard",
                placement = "bottom", add_head = attr(value, "data_head"))
    updateAceEditor(session, "edit_data", value = value)
  })

  # Clear editor when input button pressed
  observeEvent(input$clear_button, {
    clear_editor()
    updateSelectInput(session, "manual_select_data", selected = NA)
  })


  # --- Download image on button press ----

  output$download_image_button = downloadHandler(
    filename = function()
      paste0("breathtest_", get_data()$patient_id[1], "_", Sys.Date(), ".png"),
    content = function(file) {
      f = fit()
      if (is.null(f)) return(NULL)
      n_patient = length(unique(get_data()$patient_id))
      if ((n_patient %% ncol_facetwrap) == 1)
        ncol_facetwrap = ncol_facetwrap - 1
      p = plot(f) +
        facet_wrap(~patient_id, ncol = ncol_facetwrap) +
        theme(legend.key.size = unit(2, "line")) +
        guides(colour = guide_legend(override.aes = list(size = 2)))
      # Size is in inches
      width = (min(n_patient, ncol_facetwrap)*6 + 1.5)/1.4
      height = plot_height()/50
      ggsave(file, p, width = width, height = height)
    }
  )

  # Retrieve data from editor
  get_data = reactive({
    data = input$edit_data
    req(data)
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
        (input$fit_method != "stan_group") || (length(unique(d$group)) > 1L),
        "Multiple groups required. Try individual curve fit or Bayesian fit instead."
      )
    ) # end validate
    d
  })

  # Compute fit when button pressed or method changed
  fit = reactive({
    method = input$fit_method
    updateTabsetPanel(inputId = "main_panel", selected = ns("data_panel"))
    data = get_data()
    req(data)
    ret = try(switch(
      method,
      data_only = null_fit(data),
      nls = nls_fit(data),
      nlme = nlme_fit(data),
      stan = stan_fit(# in package breathteststan
        data,
        chains = 2,
        student_t_df = as.integer(input$student_t_df),
        iter = as.integer(input$iter)
      ),
      stan_group = stan_group_fit(# in package breathteststan
        data,
        chains = 2,
        student_t_df = as.integer(input$student_t_df),
        iter = as.integer(input$iter)
      )
    ), silent = FALSE)
    ret
  })

  # Returns coefficients of fit and comment
  coef_fit = function() {
    f  = fit()
    if (is.null(f) || inherits(f, "try-error"))
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
    req(!inherits(f, "breathtestnullfit"))
    cf =  try(coef_by_group(f), silent = TRUE)
    req(!inherits(cf, "try-error"))
    bt_datatable(cf)
  })

  output$coef_by_group_diff_table = DT::renderDataTable({
    f = fit()
    req(!inherits(f, "breathtestnullfit"))
    cf =  try(coef_diff_by_group(fit()), silent = TRUE)
    req(!inherits(cf, "try-error"))
    bt_datatable(cf)
  })

  # --------- outputs -------
  # Plots
  plot_height = function() {
    n_patient = length(unique(get_data()$patient_id))
    n_patient %/% ncol_facetwrap * 130L + 200L
  }

  output$fit_plot = renderPlot({
    f = fit()
    req(f)
    plot(f) +
      facet_wrap(~patient_id, ncol = ncol_facetwrap) +
      theme(aspect.ratio = 0.8)
  }, height = plot_height)


  # Panel logic --------------------
  observe({
    req(input$data_source)
    data_source = input$data_source
    data_subset = isolate(input$data_subset)
    updateSelectInput(session, "data_subset",
                        choices = data_subsets[[data_source]],
                        selected = "cross_over_5")
  })

  # TODO Join with above?
  observe({
    data_subset = input$data_subset
    data_source = isolate(input$data_source)
    req(data_subset, data_source)
    if (data_subset == "manual")
      updateSelectInput(session, "manual_select_data",
                        choices = manual_subsets[[data_source]])
  })


  # ------------- Hide panel logic --------------------
  observe({
    cf = coef_fit()
    has_fit = input$fit_method != "data_only" && !is.null(cf)

    toggle(
      condition = has_fit,
      selector = list(
        glue("a[data-value={ns('details_panel')}]"),
        glue("a[data-value={ns('summary_panel')}]")
      )
    )
    has_groups = if (!has_fit || is.null(cf)) FALSE else
                        length(unique(cf$group)) > 1
    toggle(condition = has_groups,
           selector = glue("a[data-value={ns('group_differences_panel')}]"))
  })

  # ------------- Help-related functions --------------------

  # Clear sample data selection when patient data are changed
  observeEvent(input$manual_select_data, {
    updateSelectInput(session, "simulated_data", selected = NA)
  })

  observe({
    shinyjs::toggle("download_filtered", condition = !is.null(coef_fit()))
  })

  observe({
    pop_control(session, input,  "iter", "Number of iterations Stan sampling")
    pop_control(session, input, "download_filtered",
                "Download coefficients as CSV-file")
    pop_control(session, input,  "student_t_df", "Outlier handling")
    pop_control(session, input,  "upload", "Upload breathtest data")
    pop_control(session, input,  "append", "Append data in editor")

    pop_select(session, input,  "fit_method", "Fitting method")
    pop_select(session, input, "data_source", "Data source")
    pop_select(session, input,  "data_subset", "Sample data")
  }) %>%
  bindEvent(input$show_pop, input$fit_method, input$data_subset, input$data_source)


  # --------------- Uploading files -----------------------------------------
  dt_list = reactive({
    in_file <- input$upload # When upload changes
    if (is.null(in_file)) return(NULL)
    in_file$status = NA
    dt_list = list()
    n_files = nrow(in_file)
    if (n_files == 0) return(NULL)
    for (i in 1:n_files) {
      # Restore original filename for better messaging
      src_file = in_file[i, "datapath"]
      dest_file = file.path(dirname(src_file), in_file[i, "name"])
      suppressWarnings(file.remove(dest_file)) # In case it exists
      file.rename(src_file, dest_file)
      # Read file
      dt = try(read_any_breathtest(dest_file), silent = FALSE)
      if (length(dt) == 0) {
        showNotification(paste("File", in_file[i, 1], "format is not valid"),
                         type = "error")
        clear_upload()
      } else if (inherits(dt, "try-error")) {
        in_file[i, "status"] = str_replace(dt, dirname(src_file), "")
      } else {
        in_file[i, "status"] = "Ok"
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
      updateCheckboxGroupInput(session, "select_records", selected = character(0))
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
        dt = str_replace(dt, ".*?\\n", "")
        dt = str_c(dt_old, "\n", dt)
      }
      updateAceEditor(session, "edit_data", value = dt)
    }
  })

  patient_modal = function(dt_s, failed = FALSE) {
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
} # Server
