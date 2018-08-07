# Libraries are included to keep intellisense quiet
suppressPackageStartupMessages(library(shinyjs))
library(shinyAce)
library(shinyBS)
library(shinythemes)
library(shinycssloaders)

shinyUI(
  fluidPage(
    theme = shinytheme("simplex"),
    useShinyjs(),
    extendShinyjs(text = jsCode),
    tags$link(rel = "stylesheet", type = "text/css", href = "breathtestshiny.css"),
    singleton(tags$head(tags$script(src = "message-handler.js"))),
    titlePanel("Gastric emptying from 13C Breath Test Data"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "fit_method",
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
          selectInput(
            "iter",
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
                "upload",
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
            checkboxInput("append", "Append selected data", value = FALSE),
            value = "uploads_panel"
          ),
          # End uploads_panel

          tabPanel(
            title = "Demo",
            conditionalPanel(
              "input.method_a == 'stan' |input.fit_method == 'stan_group'",
              selectInput(
                "student_t_df",
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
              "data_source",
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
              "data_subset",
              "Data subset",
              choices = NULL,
              selected = NULL
            ),
            conditionalPanel(
              "input.data_subset == 'manual'",
              selectInput(
                "manual_select_data",
                "Select data",
                choices = NULL,
                multiple = TRUE
              )
            ),
            value = "demo_panel"
          ), # end demo_panel
          id = "samples_panel"
        ),
        # End tabsetpanel
        # actionLink("create_workspace", "Create Workspace"),
        hr(),
        HTML('<a href="https://dmenne.github.io/breathtestcore/articles/data_formats.html#vendor-specific-formats">Supported Formats</a>'),
        # The following should not be moved to the server
        bsPopover("show_pop",  "Enable/disable all popups", "", "right"),
        checkboxInput("show_pop", "Show popover help", value = FALSE),
        actionLink("about", "About"),
        width = 3
      ),
      # end sidebarPanel
      mainPanel(
        tabsetPanel(
          id = "main_panel",
          tabPanel(
            title = "Data",
            aceEditor("edit_data", "", mode = "plain_text"),
            actionButton("clear_button", "Clear", icon = icon("eraser")),
            downloadButton("download_image_button", "Image"),
            tags$script(type = "text/javascript", HTML(ace_options)),
            withSpinner(plotOutput("fit_plot", height = "auto")),
            hr(),
            bsModal(
              "about_tab",
              "About breathtestshiny",
              "about",
              size = "large",
              HTML(about_text)
            ),
            value = "data_panel"
          ),
          # Data tabPanels
          tabPanel(
            title = "Details",
            value = "details_panel",
            clear_search_text("details"),
            h2("Per Patient/Record results"),
            withSpinner(DT::dataTableOutput("coef_table"))
          ),
          tabPanel(
            title = "Summary",
            value = "summary_panel",
            clear_search_text("summary"),
            h2("Per Group Means"),
            withSpinner(DT::dataTableOutput("coef_by_group_table"))
          ),
          # End Summary tabPanel
          tabPanel(
            title = "Group differences",
            value = "group_differences_panel",
            clear_search_text("group_differences"),
            h2("Difference means between groups"),
            withSpinner(DT::dataTableOutput("coef_by_group_diff_table"))
          ) # End Group differences tabPanel
        ) # end tabsetPanel
      ) # end mainPanel
    ) # end sidebarLayout
  ) # fluidpage
) # shinyUI
