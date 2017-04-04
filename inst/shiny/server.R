library(shinyjs)
shinyServer(function(input, output, session) {

  uid = reactive({
    u = parseQueryString(session$clientData$url_search)[["uid"]]
    # Make it a valid subdirectory name
    cc = cleanup_uid(u)
    cc
  })

  data_dir = function(){
    if (is.null(uid()))
      return(NULL)
    file.path(data_root, uid())
  }

  url = reactive({
    cd = session$clientData
    url_hostname = cd$url_hostname
    url_port = cd$url_port
    url1 = paste0(url_hostname, ":", url_port)
    dd = data_dir()
    if (!is.null(dd)) {
      safe_dir_create(data_root)
      safe_dir_create(dd)
      url1 = paste0(url1, "/?uid=", uid())
    }
    url1
  })

  store_path = reactive({
    parseQueryString(session$clientData$url_search)[["uid"]]
  })

  observe({
    shinyjs::toggleState("create_workspace", is.null(uid()))
  })

  observe({
    updateActionButton(session, "userid", url() )
  })

  observe({
    if (input$create_workspace == 0)
      return(NULL)
    new_uid = paste0("?uid=", digest::digest(rnorm(1), "xxhash32" ))
    session$sendCustomMessage(type = 'replace_message',
            message = new_uid )
  })

  observe({
    show = input$show_pop
    popit(session, show, "method_a", "Fitting method")
  })

  # A histogram
  output$fit_plot <- renderPlot({
    hist(rnorm(100), main = "Platzhalter")
  })
})
