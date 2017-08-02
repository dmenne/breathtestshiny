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
              #,  "Grouped Bayesian fit" = "stan_group"
            ),
          selected = "data_only"
        ),
        conditionalPanel(
          "input.method_a == 'stan' |input.method_a == 'stan_group'",
          selectInput(
            "iter",
            "Iterations",
            choices = c(200, 500, 1000, 2000),
            selected = 200
          ),
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
        conditionalPanel(
          "input.showsamples == 1",
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
          )
        ),
        conditionalPanel(
          "input.showsamples == 1",
          selectInput(
            "data_subset",
            "Data subset",
            choices = NULL,
            selected = NULL
          )
        ),
        conditionalPanel("input.data_subset == 'manual' & input.showsamples == 1",
        selectInput(
          "manual_select_data",
          "Select data",
          choices = NULL,
          multiple = TRUE
        )
      ),
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
        id = "main_panel",
        tabPanel(
          title = "Data",
          aceEditor("edit_data", "", mode = "plain_text"),
          actionButton("clear_button", "Clear", icon = icon("eraser")),
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
          withSpinner(DT::dataTableOutput("coef_table"))
        ),
        tabPanel(
          title = "Summary",
          value = "summary_panel",
          clear_search_text("summary"),
          withSpinner(DT::dataTableOutput("coef_by_group_table"))
        ),
        # End Summary tabPanel
        tabPanel(
          title = "Group differences",
          value = "group_differences_panel",
          clear_search_text("group_differences"),
          withSpinner(DT::dataTableOutput("coef_by_group_diff_table"))
        ) # End Group differences tabPanel
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidpage
) # shinyUI
