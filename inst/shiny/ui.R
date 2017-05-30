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
    tags$link(rel = "stylesheet", type = "text/css", href = "breathtestshiny.css"),
    singleton(tags$head(tags$script(src = "message-handler.js"))),
    titlePanel("Gastric emptying from 13C Breath Test Data"),
    sidebarLayout(
      sidebarPanel(
        h3("Analyze data"),
        selectInput(
          "method_a",
          "Method",
          choices =
            c(
              "No fit, data only" = "data_only",
              "Individual curve fit (nls)" = "nls",
              "Mixed-model fit (nlme) " = "nlme",
              "Bayesian fit (Stan)" = "stan"
            ),
          selected = "nls"
        ),
        conditionalPanel("input.method_a == 'stan'",
            selectInput("iter", "Iterations", choices = c(200,500, 1000, 2000),
                        selected = 200),
            selectInput("student_t_df", "Expected outliers",
                        choices = c("None - Gaussian" = 10,
                                    "Few - Student-t 5 df" = 5,
                                    "Strong - Student-t 3 df" = 3))
        ),
        conditionalPanel("input.showsamples == 1",
          selectInput("sample_data", "Sample data",
            list("",
                 "One record without header" = "no_header",
                 "One record with header" = "with_header",
                 "Records from 2 patients" = "two_patients",
                 "Crossover from one patient" = "cross_over",
                 "Larger data set" = "large_set",
                 "Very large set" = "very_large_set"),
                selected = ""),
          selectInput("patient_test_data", "Patient test data",
            list(`Easy normals, solid and liquid` = c("norm_001", "norm_002", "norm_003"),
                 `Easy patients` = c("pat_001", "pat_002", "pat_003"),
                 `Difficult patients` = c("pat_051", "pat_016", "pat_033")),
                  multiple = TRUE,
                  selected = c("norm_001", "norm_002", "norm_003","pat_001", "pat_003","pat_016"))
        ), # conditionalPanel input.showsample
        checkboxInput("showsamples", "Show sample data sets", TRUE),
        textOutput("use_link"),
        actionLink("userid", ""),
        checkboxInput("show_pop", "Show popover help", value = TRUE),
        # The following should not be moved to the server
        bsPopover("show_pop",  "Enable/disable all popups", "", "right"),
#        bsButton("create_workspace", "Keep data"),
        hr(),
        actionLink("about", "About"),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Data",
            aceEditor("edit_data", "", mode = "plain_text"),
            actionButton("clear_button", "Clear", icon = icon("eraser")),
            tags$script(type = "text/javascript",HTML(ace_options)),
            withSpinner(plotOutput("fit_plot", height = "auto")),
            hr(),
            bsModal("about_tab", "About breathtestshiny", "about",
                    size = "large", HTML(about_text))
          ),
          # Data tabPanels
          tabPanel("Details",
             DT::dataTableOutput("coef_table")
          ),
          tabPanel("Summary",
            withSpinner(DT::dataTableOutput("coef_by_group_table"))
          ), # End Summary tabPanel
          tabPanel("Group differences",
            withSpinner(DT::dataTableOutput("coef_by_group_diff_table"))
          ) # End Group differences tabPanel
        ) # tabsetPanel
      ) # mainPanel
    ) # sidebarLayout
  ) # fluidpage
) # shinyUI
