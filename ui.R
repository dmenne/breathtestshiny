shinyUI(pageWithSidebar(
  headerPanel("Shiny Client Data"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500),
    helpText("Use this link to recover your data"),
    actionLink("userid", base_url),
    singleton(
      tags$head(tags$script(src = "message-handler.js"))
    )

  ),
  mainPanel(
    h3("clientData values"),
    plotOutput("myplot")
  )
))
