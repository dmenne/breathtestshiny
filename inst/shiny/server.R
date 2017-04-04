library(shinyjs)
shinyServer(function(input, output, session) {

  uid = reactive({
    u = parseQueryString(session$clientData$url_search)[["uid"]]
    # Make it a valid subdirectory name
    cleanup_uid(u)
  })

  data_dir = function(){
    u = uid()
    ifelse(is.null(u), NULL, file.path(data_root, u))
  }

  url = reactive({
    cd = session$clientData
    url1 = paste0(cd$url_hostname, ":", cd$url_port)
    ifelse(is.null(uid()), url1, paste0(url1, "/?uid=", uid()))
  })

  observe({
    shinyjs::toggle("create_workspace", condition = is.null(uid()))
    shinyjs::toggle("userid", condition = !is.null(uid()))
  })

  observe({
    updateActionButton(session, "userid", url() )
  })

  observe({
    if (input$create_workspace == 0)
      return(NULL)
    new_uid = paste0("?uid=", digest::digest(rnorm(1), "xxhash32" ))
    session$sendCustomMessage(type = 'replace_url',
            message = new_uid )
  })

  output$use_link = reactive({
    ifelse(is.null(uid()), "", "Use this link to recover data")
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
