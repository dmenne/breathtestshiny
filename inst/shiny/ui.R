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
    titlePanel("Fit 13C Breath Test"),
    sidebarLayout(
      sidebarPanel(
        h3("Analyze data"),
        selectInput(
          "method_a",
          "Method",
          choices =
            c(
              "single curve fit" = "nls",
              "nlme population fit" = "nlme",
              "Bayesian Stan fit" = "stan"
            ),
          selected = "nls"
        ),
        selectInput("test_data", "Select test data:",
          list(`Easy normals` = c("norm_001", "norm_002", "norm_003"),
               `Easy patients` = c("pat_001", "pat_002", "pat_003"),
               `Difficult patients` = c("pat_021", "pat_023", "pat_032")),
        multiple = TRUE),
        textOutput("use_link"),
        actionLink("userid", ""),
        actionButton("create_workspace", "Keep data"),
        actionButton("test", "Test"),
        checkboxInput("show_pop", "Show popover help", value = TRUE),
        # The following should not be moved to the server
        bsPopover("show_pop",  "Enable/disable all popups", "", "right"),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Data",
            aceEditor("data", "", mode = "plain_text"),
            actionButton("clearButton", "Clear", icon = icon("eraser")),
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
