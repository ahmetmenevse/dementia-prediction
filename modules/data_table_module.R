library(shiny)
library(DT)

dataTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("table")),
    hr(),
    p("For next step: Please proceed to the 'Missing Values' tab above to explore the dataset in detail.")
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
