shinyServer(function(input, output, session) {

  uid = reactive({
    u = parseQueryString(session$clientData$url_search)[["uid"]]
    # Make it a valid subdirectory name
    cc = cleanup_uid(u)
    cc
  })

  data_dir = function(){
    if (is.null(uid()) || uid() == "")
      return(NULL)
    file.path(data_root, uid())
  }

  observe({
    cd = session$clientData
    url_hostname = cd$url_hostname
    url_port = cd$url_port
    url = paste0(url_hostname, ":", url_port)
    dd = data_dir()
    if (!is.null(dd)) {
      safe_dir_create(data_root)
      safe_dir_create(dd)
      url = paste0(url, "/?uid=", uid())
    }
    updateActionButton(session, "userid", url )
  })

  # A histogram
  output$myplot <- renderPlot({
    hist(rnorm(input$obs), main="h")
  })
})
