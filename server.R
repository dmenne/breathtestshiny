shinyServer(function(input, output, session) {

  observe({
    url_s = parseQueryString(session$clientData$url_search)
    uid = url_s[["uid"]]
    if (is.null(uid)) {
      uid = digest::digest(rnorm(1),"xxhash64")
    }
    url = paste0(base_url, "/?uid=", uid)
    updateActionButton(session, "userid", label = url)
  })

  observe({
    session$clientData$url_search
    session$sendCustomMessage(type = 'testmessage',
                    message = '')
  })

  # A histogram
  output$myplot <- renderPlot({
    hist(rnorm(input$obs), main="h")
  })
})
