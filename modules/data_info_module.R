dataInfoUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3('Dataset Description'),
        p("The dataset, sourced from a mobile healthcare service operated by non-governmental organizations managing elderly care centers in Hong Kong between 2008-2018, contains various demographic, health, and quality of life information for elderly individuals. 
        It includes 2299 observations and 12 variables, covering age, gender, height, weight, education level, financial status, and nutrition assessment scores."),
        hr(),
        selectInput(ns('variable_type'), 'Select variable type:', choices = c('Categorical', 'Continuous')),
        uiOutput(ns('variable_select'))
      ),
      mainPanel(
        textOutput(ns('selected_var')),
        tableOutput(ns('variable_summary'))
      )
    )
  )
}

dataInfoServer <- function(id, data, categorical_vars, continuous_vars, category_labels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$variable_select <- renderUI({
      if (input$variable_type == 'Categorical') {
        selectInput(ns('variable'), 'Selected Variable: ', choices = categorical_vars)
      } else {
        selectInput(ns('variable'), 'Selected Variable: ', choices = continuous_vars)
      }
    })
    
    output$selected_var <- renderText({
      paste('Selected variable: ', input$variable)
    })
    
    output$variable_summary <- renderTable({
      req(input$variable)  
      selected_var <- input$variable
      data_df <- data() 
      
      if (!is.null(selected_var)) {
        if (input$variable_type == 'Categorical') {
          summary_stats <- as.data.frame(table(data_df[[selected_var]], useNA = "ifany"))
          colnames(summary_stats) <- c("Category", "Frequency")
          
          if (selected_var %in% names(category_labels)) {
            summary_stats$Category <- category_labels[[selected_var]][as.character(summary_stats$Category)]
          }
        } else {
          summary_stats <- summary(data_df[[selected_var]])
          summary_stats <- data.frame(Feature = names(summary_stats), Value = as.character(summary_stats))
        }
        summary_stats
      }
    })
  })
}
