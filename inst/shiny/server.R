library(shinyjs)
library(shinyAce)
library(breathtestcore)
library(dplyr)


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
    url1 = cd$url_hostname
    if (!is.null(cd$url_port))
      url1 = paste0(url1, ":", cd$url_port)
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
    # Clear ace editor
    if (input$clearButton == 0)
      return(NULL)
    updateAceEditor(session, "data",value = 1)
    updateSelectizeInput(session, "test_data", selected = NA)

  })


  observe({
    # Retrieve data
    td = input$test_data
    if (is.null(td)) return(NULL)
    data("usz_13c", envir = environment())
    data = usz_13c  %>%
      filter(pat_id %in% td) %>%
      mutate(
        pdr = round(pdr, 1)
      )
    tc = textConnection("dt", "w")
    #comment = str_replace_all(comment(data),"\\n", " ")
    comment = "Subset of USZ 13C data"
    writeLines(paste0("# ", comment), con = tc)
    writeLines(paste0("# ", paste0(input$test_data, collapse = ", ")), con = tc)
    suppressWarnings(write.table(data, file = tc, append = TRUE,
              row.names = FALSE, sep = "\t", quote = FALSE))
    updateAceEditor(session, "data", value = paste(dt, collapse = "\n"))
    close(tc)
  })


  observe({
    show = input$show_pop
    popit(session, show, "method_a", "Fitting method")
    popit(session, show, "test_data", "Sample test data")
  })

  # A histogram
  output$fit_plot <- renderPlot({
    hist(rnorm(100), main = "Platzhalter")
  })
})
