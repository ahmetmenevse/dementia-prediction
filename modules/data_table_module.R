library(shiny)
library(DT)

dataTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("table"))
  )
}

dataTableServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$table <- renderDT({
      datatable(data(), options = list(pageLength = 10, autoWidth = TRUE), filter = 'top')
    })
  })
}
