# Libraries are included to keep intellisense quiet
suppressPackageStartupMessages(library(shinyjs))
library(shinyAce)
library(shinyBS)

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "bootstrap.css",
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
              "Data only, no fit" = "data_only",
              "Individual curve fit (nls)" = "nls",
              "Mixed-model fit (nlme) " = "nlme",
              "Bayesian fit (Stan)" = "stan"
            ),
          selected = "data_only"
        ),
        selectInput("select_test_data", "Select test data:",
          list(`Easy normals, solid and liquid` = c("norm_001", "norm_002", "norm_003"),
               `Easy patients` = c("pat_001", "pat_002", "pat_003"),
               `Difficult patients` = c("pat_051", "pat_016", "pat_033")),
                multiple = TRUE,
                selected = c("norm_001", "norm_002", "norm_003")),
        textOutput("use_link"),
        actionLink("userid", ""),
        actionButton("create_workspace", "Keep data"),
        checkboxInput("show_pop", "Show popover help", value = TRUE),
        # The following should not be moved to the server
        bsPopover("show_pop",  "Enable/disable all popups", "", "right"),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Data",
            aceEditor("edit_data", "", mode = "plain_text"),
            actionButton("clear_button", "Clear", icon = icon("eraser")),
            tags$script(type = "text/javascript",HTML(ace_options)),
            div(
              id = "plot-container",
              tags$img(src = "spinner.gif",
                       id = "loading-spinner"),
              plotOutput("fit_plot")
            ),
            hr(),
            downloadButton("download_coef", "Download"),
            DT::dataTableOutput("table")
          ),
          # tabPanel
          tabPanel("Check",
                   plotOutput("residual_plot"),
                   plotOutput("trace_v")) # check tabPanel
        ) # tabsetPanel
      ) # mainPanel
    ) # sidebarLayout
  ) # fluidpage
) # shinyUI
