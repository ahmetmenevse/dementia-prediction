library(shiny)
library(ggplot2)
library(dplyr)

# UI for the Visualization Module
visualizationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             h3("Categorical Variables Visualization")
      )
    ),
    fluidRow(
      uiOutput(ns("categorical_plots_ui"))
    ),
    fluidRow(
      column(12,
             h3("Continuous Variables Visualization")
      )
    ),
    fluidRow(
      uiOutput(ns("continuous_plots_ui"))
    )
  )
}

# Server function for the Visualization Module
visualizationServer <- function(id, data, categorical_vars, continuous_vars, category_labels, variable_labels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$categorical_plots_ui <- renderUI({
      data_df <- data()
      plot_list <- lapply(categorical_vars, function(var) {
        plot_output_id <- ns(paste0("plot_", var))
        plotOutput(plot_output_id, height = "300px", width = "400px")
      })
      
      # Create a fluid row for each set of 3 plots to maintain layout
      tagList(
        lapply(seq(1, length(plot_list), by = 3), function(i) {
          fluidRow(
            column(4, plot_list[[i]]),
            column(4, if (i + 1 <= length(plot_list)) plot_list[[i + 1]]),
            column(4, if (i + 2 <= length(plot_list)) plot_list[[i + 2]])
          )
        })
      )
    })
    
    output$continuous_plots_ui <- renderUI({
      data_df <- data()
      plot_list <- lapply(continuous_vars, function(var) {
        plot_output_id <- ns(paste0("plot_", var))
        plotOutput(plot_output_id, height = "300px", width = "400px")
      })
      
      # Create a fluid row for each set of 3 plots to maintain layout
      tagList(
        lapply(seq(1, length(plot_list), by = 3), function(i) {
          fluidRow(
            column(4, plot_list[[i]]),
            column(4, if (i + 1 <= length(plot_list)) plot_list[[i + 1]]),
            column(4, if (i + 2 <= length(plot_list)) plot_list[[i + 2]])
          )
        })
      )
    })
    
    observe({
      data_df <- data()
      lapply(categorical_vars, function(var) {
        output[[paste0("plot_", var)]] <- renderPlot({
          plot_data <- data_df %>% filter(!is.na(.data[[var]]))
          levels <- names(category_labels[[var]])
          plot_data[[var]] <- factor(plot_data[[var]], levels = levels) 
          
          ggplot(plot_data, aes(x = .data[[var]], fill = .data[[var]])) + 
            geom_bar() + 
            theme_classic() +  
            labs(title = variable_labels[[var]], x = NULL, y = "Count") + 
            scale_fill_manual(values = setNames(rainbow(length(levels)), levels), labels = category_labels[[var]]) +
            theme(
              legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
              plot.title = element_text(size = 14, face = "bold"), 
              axis.title = element_text(size = 12, face = "bold"), 
              legend.title = element_text(size = 12, face = "bold"), 
              legend.text = element_text(size = 10),  
              plot.margin = ggplot2::margin(10, 10, 10, 10)
            ) +
            guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3) 
        })
      })
      
      lapply(continuous_vars, function(var) {
        output[[paste0("plot_", var)]] <- renderPlot({
          plot_data <- data_df %>% filter(!is.na(.data[[var]]))
          ggplot(plot_data, aes(x = .data[[var]])) + 
            geom_density(adjust = 1.5, fill = "blue", alpha = 0.3) +
            theme_classic() +  
            labs(title = variable_labels[[var]], x = variable_labels[[var]], y = "Density") +
            theme(
              plot.title = element_text(size = 14, face = "bold"),  
              axis.title = element_text(size = 12, face = "bold")
            )
        })
      })
    })
  })
}
