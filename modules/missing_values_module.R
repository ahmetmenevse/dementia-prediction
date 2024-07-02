library(shiny)
library(shinythemes)
library(dplyr)
library(VIM)

missingValuesUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Missing Values Information"),
        p(strong("Mean: "), "Replaces missing values with the mean of the column."),
        p(strong("Median: "), "Replaces missing values with the median of the column."),
        p(strong("KNN: "), "Uses K-nearest neighbors algorithm to impute missing values."),
        p(strong("Delete: "), "Removes rows with missing values."),
        p(strong("Mode: "), "Replaces missing values with the mode of the column."),
        p(strong("Hot Deck: "), "Replaces missing values using similar records from the dataset."),
        
        h4("Continuous Variables"),
        p("Select a method to impute missing values for continuous variables."),
        radioButtons(ns('cont_method'), 'Select Method', choices = c('Mean', 'Median', 'KNN', 'Delete')),
        
        h4("Categorical Variables"), 
        p("Select a method to impute missing values for categorical variables."),
        radioButtons(ns('cat_method'), 'Select Method', choices = c('Mode', 'KNN', 'Hot Deck', 'Delete')),
        actionButton(ns('impute'), 'Impute Missing Values')
      ),
      mainPanel(
        h3("Missing Values Summary"),
        tableOutput(ns('missing_summary')),
        h3("Imputation Details"),
        tableOutput(ns('imputation_details')),
        h3("Comparison Summary"),
        tableOutput(ns('comparison_summary'))
      )
    )
  )
}

missingValuesServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    missing_summary <- data.frame(
      Variable = names(data),
      Missing_Count = sapply(data, function(x) sum(is.na(x))),
      Missing_Percent = sapply(data, function(x) mean(is.na(x))) * 100
    )
    
    output$missing_summary <- renderTable({
      missing_summary
    })
    
    observeEvent(input$impute, {
      cont_method <- input$cont_method
      cat_method <- input$cat_method
      
      if (is.null(cont_method) || is.null(cat_method)) {
        return(NULL)
      }
      
      data_copy <- data
      
      imputation_details <- list()
      
      missing_cont_vars <- continuous_vars[sapply(data[continuous_vars], function(x) any(is.na(x)))]
      missing_cat_vars <- categorical_vars[sapply(data[categorical_vars], function(x) any(is.na(x)))]
      
      if (length(missing_cont_vars) > 0 && !is.null(cont_method)) {
        if (cont_method == 'Mean') {
          for (var in missing_cont_vars) {
            mean_value <- mean(data_copy[[var]], na.rm = TRUE)
            data_copy[[var]][is.na(data_copy[[var]])] <- mean_value
            imputation_details[[var]] <- paste("Mean: ", round(mean_value, 2))
          }
        } else if (cont_method == 'Median') {
          for (var in missing_cont_vars) {
            median_value <- median(data_copy[[var]], na.rm = TRUE)
            data_copy[[var]][is.na(data_copy[[var]])] <- median_value
            imputation_details[[var]] <- paste("Median: ", median_value)
          }
        } else if (cont_method == 'KNN') {
          valid_cols <- missing_cont_vars[sapply(data_copy[missing_cont_vars], function(x) !all(is.na(x)))]
          knn_result <- kNN(data_copy[valid_cols], k = 5)
          for (var in valid_cols) {
            data_copy[[var]] <- knn_result[[var]]
            imputation_details[[var]] <- "KNN Imputation"
          }
        } else if (cont_method == 'Delete') {
          data_copy <- data_copy[complete.cases(data_copy[, missing_cont_vars]), ]
        }
      }
  
      if (length(missing_cat_vars) > 0 && !is.null(cat_method)) {
        if (cat_method == 'Mode') {
          for (var in missing_cat_vars) {
            mode_value <- names(sort(table(data_copy[[var]]), decreasing = TRUE))[1]
            data_copy[[var]][is.na(data_copy[[var]])] <- mode_value
            imputation_details[[var]] <- paste("Mode: ", mode_value)
          }
        } else if (cat_method == 'KNN') {
          valid_cols <- missing_cat_vars[sapply(data_copy[missing_cat_vars], function(x) !all(is.na(x)))]
          knn_result <- kNN(data_copy[valid_cols], k = 5)
          for (var in valid_cols) {
            data_copy[[var]] <- knn_result[[var]]
            imputation_details[[var]] <- "KNN Imputation"
          }
        } else if (cat_method == 'Hot Deck') {
          valid_cols <- missing_cat_vars[sapply(data_copy[missing_cat_vars], function(x) !all(is.na(x)))]
          if (length(valid_cols) > 0) {
            hotdeck_result <- hotdeck(data_copy[, valid_cols, drop = FALSE], variable = valid_cols)
            for (var in valid_cols) {
              if (!is.null(hotdeck_result[[var]])) {
                data_copy[[var]] <- hotdeck_result[[var]]
                imputation_details[[var]] <- "Hot Deck Imputation"
              }
            }
          }
        } else if (cat_method == 'Delete') {
          data_copy <- data_copy[complete.cases(data_copy[, missing_cat_vars]), ]
        }
      }
      
      imputation_details_df <- data.frame(
        Variable = names(imputation_details),
        Method = unlist(imputation_details)
      )
      
      output$imputation_details <- renderTable({
        imputation_details_df
      })
  
      imputed_vars <- c(missing_cont_vars, missing_cat_vars)
      comparison_summary <- data.frame(
        Variable = imputed_vars,
        Missing_Before = sapply(data[imputed_vars], function(x) sum(is.na(x))),
        Missing_After = sapply(data_copy[imputed_vars], function(x) sum(is.na(x))),
        Mean_Before = sapply(data[imputed_vars], function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
        Mean_After = sapply(data_copy[imputed_vars], function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
        Median_Before = sapply(data[imputed_vars], function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
        Median_After = sapply(data_copy[imputed_vars], function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA)
      )
      
      output$comparison_summary <- renderTable({
        comparison_summary
      })
    })
  })
}
