box::use(
  shinyBS[addPopover, removePopover],
  shiny[tags, HTML],
  utils[head, write.table, str],
  xtable[xtable]
)
# constants
box::use(
  app/logic[pop_content],
)

#' @export
pop_control = function(session, input,  id, title, placement = "right",
                       add_head = NULL) {
  ns = session$ns
  removePopover(session, ns(id))
  if (input$show_pop) {
    content = pop_content[id]
    if (!is.null(add_head) && is.data.frame(add_head)) {
      content = paste(content,
       readr::format_delim(head(add_head, 3), delim = "\t",
                           eol = "<br>", quote = "none"),
       "...</pre>",
       sep = "<br>")
    }
    addPopover(session, ns(id), title, HTML(content), placement)
  }
}

#' @export
pop_select = function(session, input,  id, title, placement = "right") {
  content = as.character(pop_content[input[[id]]])
  if (is.na(content)) content = ""
  ns = session$ns
  removePopover(session, ns(id))
  if (input$show_pop) {
    addPopover(session, ns(id), title, content, placement)
  }
}
